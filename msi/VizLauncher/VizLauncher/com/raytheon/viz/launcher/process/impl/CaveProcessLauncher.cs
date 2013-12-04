using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using VizLauncher.com.raytheon.viz.launcher.process;
using VizLauncher.com.raytheon.viz.launcher.environment;

namespace VizLauncher.com.raytheon.viz.launcher.process.impl
{
    public class CaveProcessLauncher : AbstractProcessLauncher
    {
        private static readonly String LOG_PREFIX = "cave_";
        private static readonly String COMMAND_LINE_ARGUMENTS = "-component thinclient";
        private static readonly String CAVE_DIRECTORY = Path.DirectorySeparatorChar + "CAVE";
        private static readonly String CAVE_EXECUTABLE =
            CAVE_DIRECTORY + Path.DirectorySeparatorChar + "cave.exe";

        public CaveProcessLauncher(VizEnvironment vizEnvironment)
            : base(vizEnvironment)
        {
        }

        protected override String getApplicationSpecificPath()
        {
            return Path.PathSeparator + this.vizEnvironment.getLocation() +
               CAVE_DIRECTORY + Path.DirectorySeparatorChar + "lib" + Path.PathSeparator;
        }

        protected override String constructProcessName(String location)
        {
            return location + CAVE_EXECUTABLE;
        }

        protected override String constructLogName(String logDate)
        {
            return LOG_PREFIX + logDate + AbstractProcessLauncher.LOG_SUFFIX;
        }

        protected override String getCommandLineArguments()
        {
            return COMMAND_LINE_ARGUMENTS;
        }
    }
}