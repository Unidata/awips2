#!/usr/bin/python3

from builtins import set
from collections import namedtuple
from xml.etree import ElementTree as ET
from xml.etree.ElementInclude import include
import argparse
import glob
import logging
import os
import shutil
import sys

logging.basicConfig(format='%(asctime)-15s %(levelname)s:  %(message)s',
                    datefmt='%Y-%m-%d %H:%M:%S',
                    level=logging.DEBUG)
log = logging.getLogger(__name__)


class UsageArgumentParser(argparse.ArgumentParser):
    """
    A subclass of ArgumentParser that overrides error() to print the
    whole help text, rather than just the usage string.
    """

    def error(self, message):
        sys.stderr.write('%s: error: %s\n' % (self.prog, message))
        self.print_help()
        sys.exit(2)


def process_args():
    DESCRIPTION = "Script to export an Eclipse target platform based on an eclipse target definition xml file"

    parser = UsageArgumentParser(description=DESCRIPTION)
    parser.add_argument("targetDef",
                        help="path to Eclipse target definition xml file"
                        )
    parser.add_argument("outputDir",
                        help="target output directory"
                        )
    parser.add_argument("-r", "--replace",
                        action="store_true",
                        dest="replaceDir",
                        help="replace contents of output directory. NOTE: This will remove all contents of the output directory!"
                        )

    return parser.parse_args()


def parseTargetDefinition(targetDef):
    try:
        parser = ET.XMLParser()
        tree = ET.parse(targetDef, parser)
        root = tree.getroot()
    except ParseError as e:
            if ErrorString(e.code) == errors.XML_ERROR_JUNK_AFTER_DOC_ELEMENT:
                log.info("Skipping xml fragment: %s", path)
            elif ErrorString(e.code) == errors.XML_ERROR_NO_ELEMENTS:
                log.info("Skipping empty xml document: %s", path)
            else:
                log.error("Error parsing file: %s \"%s\"", path, ErrorString(e.code))
            return 1

    except Exception:
        log.error("Unable to parse XML path: %s", path)
        return 1

    if root.tag != "target":
        log.info("Not a target definition xml file: %s", path)
        return 0

    locations = root.find("locations")
    location = locations.find("location")

    if (location.attrib["type"] != "Directory"):
        raise Exception("source is not a directory")

    sourceDir = location.attrib["path"]

    includeBundles = root.find("includeBundles")

    features = set()
    for feature in includeBundles.iterfind("feature"):
        features.add(feature.attrib["id"])

    plugins = set()
    for plugin in includeBundles.iterfind("plugin"):
        plugins.add(plugin.attrib["id"])

    return sourceDir, features, plugins


def parseFeatureXml(featureXml):
    try:
        parser = ET.XMLParser()
        tree = ET.parse(featureXml, parser)
        root = tree.getroot()
    except ParseError as e:
            if ErrorString(e.code) == errors.XML_ERROR_JUNK_AFTER_DOC_ELEMENT:
                log.info("Skipping xml fragment: %s", path)
            elif ErrorString(e.code) == errors.XML_ERROR_NO_ELEMENTS:
                log.info("Skipping empty xml document: %s", path)
            else:
                log.error("Error parsing file: %s \"%s\"", path, ErrorString(e.code))
            return 1

    except Exception:
        log.error("Unable to parse XML path: %s", path)
        return 1

    if root.tag != "feature":
        log.info("Not a feature xml file: %s", path)
        return 0

    featurePlugins = set()
    Plugin = namedtuple("Plugin", ["id", "version", "unpack"])
    for plugin in root.iterfind("plugin"):
        featurePlugins.add(
            Plugin(
                plugin.attrib["id"],
                plugin.attrib["version"],
                getattr(plugin.attrib, "unpack", "false") == "true"
            )
        )

    return featurePlugins


def exportTarget(sourceDir, features, plugins, outputDir):
    status = 0

    sourcePluginDir = os.path.join(sourceDir, "plugins")
    outputPluginDir = os.path.join(outputDir, "plugins")
    os.makedirs(outputPluginDir)
    for pluginId in plugins:
        status |= exportPlugin(sourcePluginDir, outputPluginDir, pluginId)

    sourceFeatureDir = os.path.join(sourceDir, "features")
    outputFeatureDir = os.path.join(outputDir, "features")
    os.makedirs(outputFeatureDir)
    for featureId in features:
        status |= exportFeature(featureId, sourceFeatureDir, sourcePluginDir, outputFeatureDir, outputPluginDir)

    return status


def exportPlugin(sourcePluginDir, outputPluginDir, pluginId, version=None):
    # locate the plugin jar/directory
    if not version:
        version = '*'
    pluginPattern = f"{pluginId}_{version}*"

    paths = glob.glob(os.path.join(sourcePluginDir, pluginPattern))
    if len(paths) == 0:
        log.error(f"Plugin: {pluginId} not found!")
        return 1

    elif len(paths) > 1 and not isJakartaException(paths):
       raise Exception(f"Multiple plugins found for plugin id: {pluginId}, paths: {paths}")
    
    # copy the plugin jar/directory to the target
    status = 0
    for srcPath in paths:
        status |= copyToTarget(outputPluginDir, srcPath)

    return status

def isJakartaException(paths):
    """
    jakarta.inject and jakarta.annotation have multiple versions packaged with eclipse 4.31.
    These are both needed because the 1.* version provides the legacy javax.* namespaces
    and 2.* version provides the new jakarta.* namespace.
    """
    return any(
        "jakarta.annotation-api" in p or 
        "jakarta.inject.jakarta.inject-api" in p 
            for p in paths
    )

def copyToTarget(outputPluginDir, srcPath):
    pluginWithVersion = os.path.split(srcPath)[1]
    dstPath = os.path.join(outputPluginDir, pluginWithVersion)
    if os.path.exists(dstPath):
        log.debug(f"{dstPath} exists, skipping...")
    else:
        log.debug(f"Exporting plugin: {pluginWithVersion}")
        if os.path.isdir(srcPath):
            shutil.copytree(srcPath, dstPath)
        else:
            shutil.copy2(srcPath, outputPluginDir)

    return 0


def exportFeature(featureId, sourceFeatureDir, sourcePluginDir, outputFeatureDir, outputPluginDir):
    # locate the feature jar/directory
    featurePattern = f"{featureId}_*"
    paths = glob.glob(os.path.join(sourceFeatureDir, featurePattern))
    if len(paths) == 0:
        log.error(f"Feature: {featureId} not found!")
        return 1

    elif len(paths) > 1:
        raise Exception(f"Multiple features found for feature id: {featureId}")

    # copy the feature directory into the target
    srcPath = paths[0]
    featureName = os.path.split(srcPath)[1]
    log.debug(f"Exporting feature: {featureName}")
    dstPath = os.path.join(outputFeatureDir, featureName)
    shutil.copytree(srcPath, dstPath)

    # get the feature's list of plugins
    featureXmlPath = os.path.join(srcPath, "feature.xml")
    featurePlugins = parseFeatureXml(featureXmlPath)
    for plugin in featurePlugins:
        exportPlugin(sourcePluginDir, outputPluginDir, plugin.id, version=plugin.version)
        if plugin.unpack:
            # TODO handle unpack
            pass

    return 0


def main():
    args = process_args()
    log.debug("Command-line args: %s", args)

    # parse the target definition
    sourceDir, features, plugins = parseTargetDefinition(args.targetDef)
    log.info(f"Extracting target from {sourceDir}...")

    # create output directory
    outputDir = args.outputDir
    if args.replaceDir and os.path.exists(outputDir):
        log.debug("Removing output directory: %s", outputDir)
        shutil.rmtree(outputDir)

    try:
        os.makedirs(outputDir)
    except FileExistsError:
        log.error("Output directory: \"%s\" already exists. Specify --replace to overwrite.", outputDir)
        return 1

    return exportTarget(sourceDir, features, plugins, outputDir)


if __name__ == '__main__':
    sys.exit(main())
