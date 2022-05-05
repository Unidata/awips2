##############################################################################
# Encryption/decryption for ignite passwords
#
# TODO RODO #8677: The ignite password encryption/decryption code in this and
# associated files is based off similar JMS password code that exists in a
# later version, so the similar code should be consolidated later on.
#
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer       Description
# ------------- -------- -------------- -------------------------------------------
# Oct 12, 2021  8667     mapeters       Initial version
# Mar 03, 2022  8762     mapeters       Handle cache VM jars being moved under
#                                       /awips2/ignite
#
##############################################################################

import os
import subprocess

class IgniteConfigurationException(Exception):
    """Exception subclass for ignite password errors."""
    pass

def updateIgnitePasswords(keystore_password, truststore_password, password_props_path):
    igniteJar = __findPluginJar("com.raytheon.uf.common.datastore.ignite")
    cryptoJar = __findPluginJar("com.raytheon.uf.common.security")
    statusJar = __findPluginJar("com.raytheon.uf.common.status")
    apacheJar = __findFossJar("org.apache.commons.codec")
    classPath = ":".join([igniteJar, cryptoJar, statusJar, apacheJar])

    passwords_dict = {'a2.ignite.keystore.password': keystore_password, 'a2.ignite.truststore.password': truststore_password}
    for password_key, password in passwords_dict.items():
        # need full java path since this is run as root, which doesn't have appropriate path vars set
        process = subprocess.run(["/awips2/java/bin/java", "-cp", classPath, "com.raytheon.uf.common.datastore.ignite.IgnitePasswordUtils", "--update", password, password_key, password_props_path],
                                 stdout=subprocess.PIPE,
                                 stderr=subprocess.PIPE)
        try:
            process.check_returncode()
        except subprocess.CalledProcessError:
            raise IgniteConfigurationException(f"Failed to update {password_key}: {process.stderr.decode()}")

def __findPluginJar(pluginName):
    # for cache VMs
    ignitePluginJar = f"/awips2/ignite/lib/plugins/{pluginName}.jar"
    if os.path.isfile(ignitePluginJar):
        return ignitePluginJar
    # for edex VMs
    edexPluginJar = f"/awips2/edex/lib/plugins/{pluginName}.jar"
    if os.path.isfile(edexPluginJar):
        return edexPluginJar
    raise RuntimeError(f"Could not locate plugin {pluginName}.jar")


def __findFossJar(libraryName):
    # for cache VMs
    igniteFossDir = f"/awips2/ignite/lib/dependencies/{libraryName}"
    if os.path.isdir(igniteFossDir):
        return f"{igniteFossDir}/*"
    # for edex VMs
    edexFossDir = f"/awips2/edex/lib/dependencies/{libraryName}"
    if os.path.isdir(edexFossDir):
        return f"{edexFossDir}/*"
    raise RuntimeError(f"Could not locate plugin {libraryName}")
