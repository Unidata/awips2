using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using Microsoft.Win32;

namespace VizLauncher.com.raytheon.viz.launcher.environment
{
    public class VizEnvironment
    {
        private static readonly String CONSOLE_LOGS_DIRECTORY = 
            Path.DirectorySeparatorChar + "caveData" + Path.DirectorySeparatorChar + 
            "logs" + Path.DirectorySeparatorChar + "consoleLogs";
        private String location;
        private String logDirectory = null;
        private String path = null;
        private String pythonPath = null;

        // did an error occur while initializing this object?
        private bool ready;
        // details about the error that has occurred.
        private String exceptionText;

        public VizEnvironment(String location)
        {
            this.location = location;
            this.init();
        }

        private void init()
        {
            /* For now we will assume that the environment properties will be available */
            // determine the location of the user's "home" directory.
            String homeDirectory = 
                this.resolveEnvironmentProperty(EnvironmentProperties.USER_HOME_ENV_PROPERTY);

            // determine the computer name.
            String computerName = 
                this.resolveEnvironmentProperty(EnvironmentProperties.COMPUTER_NAME_ENV_PROPERTY);

            // construct the path to the log directory.
            this.logDirectory = homeDirectory + CONSOLE_LOGS_DIRECTORY + 
                Path.DirectorySeparatorChar + computerName;

            // retrieve the jdk directory from the registry.
            String jdkDirectory = 
                this.retrieveRegistryProperty(EnvironmentProperties.A2_JAVA_REG, 
                EnvironmentProperties.JAVA_JRE_VALUE_NAME);
            if (jdkDirectory == null)
            {
                this.notReady("Unable to retrieve the Java JDK Path from the registry!");
                return;
            }

            // retrieve the python location from the registry.
            String pythonLocation = 
                this.retrieveRegistryProperty(EnvironmentProperties.A2_PYTHON_REG, 
                EnvironmentProperties.PYTHON_INSTALL_NAME);
            if (pythonLocation == null)
            {
                this.notReady("Unable to retrieve the Python Install Location from the registry!");
                return;
            }

            // Construct the PATH.
            this.path = pythonLocation + Path.PathSeparator;
            this.path += pythonLocation + EnvironmentProperties.PATH_PYTHON_DLLS + Path.PathSeparator;
            this.path += jdkDirectory + EnvironmentProperties.PATH_JAVA_BIN;

            // Construct the PYTHON_PATH.
            this.pythonPath = pythonLocation + EnvironmentProperties.PYTHON_PATH_PYTHON_LIBTK + Path.PathSeparator;
            this.pythonPath += pythonLocation + EnvironmentProperties.PYTHON_PATH_PYTHON_DLLS + Path.PathSeparator;
            this.pythonPath += pythonLocation + EnvironmentProperties.PYTHON_PATH_PYTHON_LIB + Path.PathSeparator;
            this.pythonPath += pythonLocation;

            this.ready = true;
        }

        private String resolveEnvironmentProperty(String property)
        {
            return Environment.ExpandEnvironmentVariables(property);
        }

        private String retrieveRegistryProperty(String registryKeyName, String valueName)
        {
            RegistryKey registryKey = Registry.LocalMachine.OpenSubKey(registryKeyName);
            if (registryKey == null)
            {
                return null;
            }
            Object registryValue = registryKey.GetValue(valueName, null);
            if (registryValue == null)
            {
                return null;
            }

            return registryValue.ToString();
        }

        private void notReady(String reason)
        {
            this.ready = false;
            this.exceptionText = reason;
        }

        public String getLocation()
        {
            return this.location;
        }

        public String getLogDirectory()
        {
            return this.logDirectory;
        }

        public String getPath()
        {
            return this.path;
        }

        public String getPythonPath()
        {
            return this.pythonPath;
        }

        public bool isReady()
        {
            return this.ready;
        }

        public String getExceptionText()
        {
            return this.exceptionText;
        }
    }
}