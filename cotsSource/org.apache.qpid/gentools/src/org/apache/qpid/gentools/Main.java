/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *
 */
package org.apache.qpid.gentools;

import org.apache.velocity.app.Velocity;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.xml.sax.SAXException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Properties;

public class Main
{
    private static final String DEFAULT_OUTPUT_DIR = ".." + Utils.FILE_SEPARATOR + "gen";
    private static final String DEFAULT_TEMPLATE_DIR_BASE = ".." + Utils.FILE_SEPARATOR;

    private enum GeneratedLanguage
    {
        CPP(".cpp", CppGenerator.getFactory()),
        DOTNET(".net", DotnetGenerator.getFactory()),
        JAVA(".java", JavaGenerator.getFactory());

        private final String _suffix;
        private final Generator.Factory _factory;


        private final String _defaultTemplateDirectory;

        GeneratedLanguage(String suffix, Generator.Factory factory)
        {
            _suffix = suffix;
            _factory = factory;
            _defaultTemplateDirectory = DEFAULT_TEMPLATE_DIR_BASE + "templ" + _suffix;
        }

        public String getSuffix()
        {
            return _suffix;
        }

        public Generator newGenerator()
        {
            return _factory.newInstance();
        }

        public String getDefaultTemplateDirectory()
        {
            return _defaultTemplateDirectory;
        }
    }

    private Generator generator;

    private String outDir;
    private String tmplDir;
    private GeneratedLanguage _generatorLang;
    private ArrayList<String> xmlFiles;

    public Main()
    {
        xmlFiles = new ArrayList<String>();
    }

    public void run(String[] args)
            throws Exception,
                   SAXException,
                   AmqpParseException,
                   AmqpTypeMappingException,
                   AmqpTemplateException,
                   TargetDirectoryException,
                   IllegalAccessException,
                   InvocationTargetException, ParserConfigurationException
    {

        // 0. Initialize
        outDir = DEFAULT_OUTPUT_DIR;
        tmplDir = null;
        _generatorLang = GeneratedLanguage.CPP; // Default generation language
        xmlFiles.clear();
        processArgs(args);

        if (tmplDir == null)
        {
            tmplDir = _generatorLang.getDefaultTemplateDirectory();
        }


        generator = _generatorLang.newGenerator();
        generator.setTemplateDirectory(tmplDir);
        generator.setOutputDirectory(outDir);

        // 1. Suck in all the XML spec files provided on the command line
        analyzeXML();

        Properties p = new Properties();
        p.setProperty("file.resource.loader.path", tmplDir);

        Velocity.init(p);

        // 2. Load up all templates
        generator.initializeTemplates();

        // 3. Generate output
        generator.generate();

        System.out.println("Files generated: " + generator.getNumberGeneratedFiles());
        System.out.println("Done.");
    }

    private void processArgs(String[] args)
    {
        // Crude but simple...
        for (int i = 0; i < args.length; i++)
        {
            String arg = args[i];
            if (arg.charAt(0) == '-')
            {
                switch (arg.charAt(1))
                {
                    case'c':
                    case'C':
                        _generatorLang = GeneratedLanguage.CPP;
                        break;
                    case'j':
                    case'J':
                        _generatorLang = GeneratedLanguage.JAVA;
                        break;
                    case'n':
                    case'N':
                        _generatorLang = GeneratedLanguage.DOTNET;
                        break;
                    case'o':
                    case'O':
                        if (++i < args.length)
                        {
                            outDir = args[i];
                        }
                        break;
                    case't':
                    case'T':
                        if (++i < args.length)
                        {
                            tmplDir = args[i];
                        }
                        break;
                }
            }
            else
            {
                xmlFiles.add(args[i]);
            }
        }
    }

    private void analyzeXML()
            throws IOException, SAXException, AmqpParseException, AmqpTypeMappingException, ParserConfigurationException
    {
        DocumentBuilder docBuilder = DocumentBuilderFactory.newInstance().newDocumentBuilder();

        System.out.println("XML files: " + xmlFiles);
        for (String filename : xmlFiles)
        {
            File f = new File(filename);
            if (f.exists())
            {
                // 1a. Initialize dom
                System.out.print("  \"" + filename + "\":");
                Document doc = docBuilder.parse(new File(filename));
                Node amqpNode = Utils.findChild(doc, Utils.ELEMENT_AMQP);

                // 1b. Extract version (major and minor) from the XML file
                int major = Utils.getNamedIntegerAttribute(amqpNode, Utils.ATTRIBUTE_MAJOR);
                int minor = Utils.getNamedIntegerAttribute(amqpNode, Utils.ATTRIBUTE_MINOR);
                AmqpVersion version = new AmqpVersion(major, minor);
                System.out.println(" Found version " + version.toString() + ".");
                generator.addVersion(version);
                generator.addFromNode(amqpNode, version);


            }
            else
            {
                System.err.println("ERROR: AMQP XML file \"" + filename + "\" not found.");
            }
        }
// *** DEBUG INFO ***  Uncomment bits from this block to see lots of stuff....
//      System.out.println();
//      System.out.println("*** Debug output ***");
//      System.out.println();
//      versionSet.print(System.out, 0, 2); // List of loaded versions
//      System.out.println();
//      constants.print(System.out, 0, 2); // List of constants
//      System.out.println();
//      domainMap.print(System.out, 0, 2); // List of domains
//      System.out.println();
//      model.print(System.out, 0, 2); // Internal version map model
//      System.out.println();
//      System.out.println("*** End debug output ***");
//      System.out.println();        
    }

    public static void main(String[] args)
    {
        int exitCode = 1;
        // TODO: This is a simple and klunky way of hangling command-line args, and could be improved upon.
        if (args.length < 2)
        {
            usage();
        }
        else
        {
            try
            {
                new Main().run(args);
                exitCode = 0;
            }
            catch (IOException e)
            {
                e.printStackTrace();
            }
            catch (ParserConfigurationException e)
            {
                e.printStackTrace();
            }
            catch (SAXException e)
            {
                e.printStackTrace();
            }
            catch (AmqpParseException e)
            {
                e.printStackTrace();
            }
            catch (AmqpTypeMappingException e)
            {
                e.printStackTrace();
            }
            catch (AmqpTemplateException e)
            {
                e.printStackTrace();
            }
            catch (TargetDirectoryException e)
            {
                e.printStackTrace();
            }
            catch (IllegalAccessException e)
            {
                e.printStackTrace();
            }
            catch (InvocationTargetException e)
            {
                e.printStackTrace();
            }
            catch (Exception e)
            {
                e.printStackTrace();
            }
        }
        System.exit(exitCode);
    }

    public static void usage()
    {
        System.out.println("AMQP XML generator v.0.0");
        System.out.println("Usage: Main -c|-j [-o outDir] [-t tmplDir] XMLfile [XMLfile ...]");
        System.out.println("       where -c:         Generate C++.");
        System.out.println("             -j:         Generate Java.");
        System.out.println("             -n:         Generate .NET.");
        System.out.println("             -o outDir:  Use outDir as the output dir (default=\"" + DEFAULT_OUTPUT_DIR + "\").");
        System.out.println("             -t tmplDir: Find templates in tmplDir.");
        System.out.println("                         Defaults: \"" + GeneratedLanguage.CPP.getDefaultTemplateDirectory() + "\" for C++;");
        System.out.println("                                   \"" + GeneratedLanguage.JAVA.getDefaultTemplateDirectory() + "\" for java.;");
        System.out.println("                                   \"" + GeneratedLanguage.DOTNET.getDefaultTemplateDirectory() + "\" for .NET.");
        System.out.println("             XMLfile is a space-separated list of AMQP XML files to be parsed.");
    }

}
