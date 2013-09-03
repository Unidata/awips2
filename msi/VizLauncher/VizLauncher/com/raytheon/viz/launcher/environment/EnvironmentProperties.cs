using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;

namespace VizLauncher.com.raytheon.viz.launcher.environment
{
    public abstract class EnvironmentProperties
    {
        /* Environment Properties */
        public static readonly String USER_HOME_ENV_PROPERTY = "%HOMEDRIVE%%HOMEPATH%";
        public static readonly String COMPUTER_NAME_ENV_PROPERTY = "%COMPUTERNAME%";

        /* Registry Constants */
        public static readonly String A2_JAVA_REG = @"Software\Raytheon\Runtime Environment\AWIPS II Java";
        public static readonly String A2_PYTHON_REG = @"Software\Raytheon\Runtime Environment\AWIPS II Python";

        public static readonly String JAVA_JRE_VALUE_NAME = "JavaJreDirectory";
        public static readonly String PYTHON_INSTALL_NAME = "PythonInstallDirectory";

        /* Environment Additions */
        public static readonly String ENVIRONMENT_VARIABLE_PATH = "Path";
        public static readonly String ENVIRONMENT_VARIABLE_PYTHON_PATH = "PythonPath";

        public static readonly String PATH_PYTHON_DLLS = Path.DirectorySeparatorChar + "DLLs";
        public static readonly String PATH_JAVA_BIN = Path.DirectorySeparatorChar + "bin";

        public static readonly String PYTHON_PATH_PYTHON_LIBTK = 
            Path.DirectorySeparatorChar + "Lib" + Path.DirectorySeparatorChar + "lib-tk";
        public static readonly String PYTHON_PATH_PYTHON_DLLS = Path.DirectorySeparatorChar + "DLLs";
        public static readonly String PYTHON_PATH_PYTHON_LIB = Path.DirectorySeparatorChar + "Lib";
    }
}
