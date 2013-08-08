using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Diagnostics;
using System.IO;
using VizLauncher.com.raytheon.viz.launcher.process;
using VizLauncher.com.raytheon.viz.launcher.environment;

namespace VizLauncher.com.raytheon.viz.launcher.process.impl
{
    public class AlertvizProcessLauncher : AbstractProcessLauncher
    {
        private static readonly String LOG_PREFIX = "alertviz_";
        private static readonly String COMMAND_LINE_ARGUMENTS = "-component thinalertviz";
        private static readonly String ALERTVIZ_PROCESS_NAME = "alertviz";
        private static readonly String ALERTVIZ_EXECUTABLE =
            Path.DirectorySeparatorChar + "AlertViz" + 
            Path.DirectorySeparatorChar + "alertviz.exe";

        public AlertvizProcessLauncher(VizEnvironment vizEnvironment) : base(vizEnvironment)
        {   
        }

        public override void launchProcess()
        {
            // need to verify that another AlertViz process is not already running.
            if (this.isAlertVizAlreadyRunning())
            {
                // do not start a new AlertViz process.
                return;
            }

            this.runProcess();
            while (this.process.ExitCode != 0)
            {
                this.runProcess();
            }
            this.closeLog();
        }

        private Boolean isAlertVizAlreadyRunning()
        {
            return (Process.GetProcessesByName(ALERTVIZ_PROCESS_NAME).Length > 0);
        }

        protected override String constructProcessName(String location)
        {
            return location + ALERTVIZ_EXECUTABLE;
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