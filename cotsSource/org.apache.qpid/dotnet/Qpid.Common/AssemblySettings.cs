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
using System;
using System.Collections;
using System.Configuration;
using System.IO;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Xml;
using log4net;

namespace Apache.Qpid.Common
{
    /// <summary>
    ///
    /// Mike Woodring
    /// Bear Canyon Consulting LLC
    /// http://www.bearcanyon.com
    ///
    /// AssemblySettings usage:
    ///
    /// If you know the keys you're after, the following is probably
    /// the most convenient:
    ///
    ///      AssemblySettings settings = new AssemblySettings();
    ///      string someSetting1 = settings["someKey1"];
    ///      string someSetting2 = settings["someKey2"];
    ///
    /// If you want to enumerate over the settings (or just as an
    /// alternative approach), you can do this too:
    ///
    ///      IDictionary settings = AssemblySettings.GetConfig();
    ///
    ///      foreach( DictionaryEntry entry in settings )
    ///      {
    ///          // Use entry.Key or entry.Value as desired...
    ///      }
    ///
    /// In either of the above two scenarios, the calling assembly
    /// (the one that called the constructor or GetConfig) is used
    /// to determine what file to parse and what the name of the
    /// settings collection element is.  For example, if the calling
    /// assembly is c:\foo\bar\TestLib.dll, then the configuration file
    /// that's parsed is c:\foo\bar\TestLib.dll.config, and the
    /// configuration section that's parsed must be named <assemblySettings>.
    ///
    /// To retrieve the configuration information for an arbitrary assembly,
    /// use the overloaded constructor or GetConfig method that takes an
    /// Assembly reference as input.
    ///
    /// If your assembly is being automatically downloaded from a web
    /// site by an "href-exe" (an application that's run directly from a link
    /// on a web page), then the enclosed web.config shows the mechanism
    /// for allowing the AssemblySettings library to download the
    /// configuration files you're using for your assemblies (while not
    /// allowing web.config itself to be downloaded).
    ///
    /// If the assembly you are trying to use this with is installed in, and loaded
    /// from, the GAC then you'll need to place the config file in the GAC directory where
    /// the assembly is installed.  On the first release of the CLR, this directory is
    /// <windir>\assembly\gac\libName\verNum__pubKeyToken]]>.  For example,
    /// the assembly "SomeLib, Version=1.2.3.4, Culture=neutral, PublicKeyToken=abcd1234"
    /// would be installed to the c:\winnt\assembly\gac\SomeLib\1.2.3.4__abcd1234 diretory
    /// (assuming the OS is installed in c:\winnt).  For future versions of the CLR, this
    /// directory scheme may change, so you'll need to check the <code>CodeBase</code> property
    /// of a GAC-loaded assembly in the debugger to determine the correct directory location.
    ///
    /// </summary>
    public class AssemblySettings
    {
        private static readonly ILog _log = LogManager.GetLogger(typeof(AssemblySettings));

        private IDictionary settings;

        [MethodImpl(MethodImplOptions.NoInlining)]
        public AssemblySettings()
            : this(Assembly.GetCallingAssembly())
        {
        }

        public AssemblySettings(Assembly asm)
        {
            settings = GetConfig(asm);
        }

        public string this[string key]
        {
            get
            {
                string settingValue = null;

                if (settings != null)
                {
                    settingValue = settings[key] as string;
                }

                return (settingValue == null ? "" : settingValue);
            }
        }

        public static IDictionary GetConfig()
        {
            return GetConfig(Assembly.GetCallingAssembly());
        }

        public static IDictionary GetConfig(Assembly asm)
        {
            // Open and parse configuration file for specified
            // assembly, returning collection to caller for future
            // use outside of this class.
            string cfgFile = asm.CodeBase + ".config";
            try
            {
                const string nodeName = "assemblySettings";
                
                XmlDocument doc = new XmlDocument();
                doc.Load(new XmlTextReader(cfgFile));

                XmlNodeList nodes = doc.GetElementsByTagName(nodeName);

                foreach (XmlNode node in nodes)
                {
                    if (node.LocalName == nodeName)
                    {
                        DictionarySectionHandler handler = new DictionarySectionHandler();
                        return (IDictionary)handler.Create(null, null, node);
                    }
                }
            }
            catch (FileNotFoundException)
            {
                _log.Warn("Assembly configuration file not found: " + cfgFile);
            }
            catch (Exception e)
            {
                _log.Warn("Failed to load .config file: " + cfgFile, e);
            }

            return null;
        }
    }
}
