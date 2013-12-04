using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using System.Diagnostics;
using VizLauncher.com.raytheon.viz.launcher.environment;

namespace VizLauncher.com.raytheon.viz.launcher.process
{
    public abstract class AbstractProcessLauncher : IProcessLauncher
    {
        private static readonly String LOG_DATE_FORMAT = "yyyyMMdd_HHmmss";
        protected static readonly String LOG_SUFFIX = ".log";
        protected Process process = null;
        private StreamWriter logFileWriter;
        protected VizEnvironment vizEnvironment;

        private bool ready = false;
        private String exceptionText = null;

        public AbstractProcessLauncher(VizEnvironment vizEnvironment)
        {
            this.vizEnvironment = vizEnvironment;
            // Prepare the log file.
            if (Directory.Exists(vizEnvironment.getLogDirectory()) == false)
            {
                Directory.CreateDirectory(vizEnvironment.getLogDirectory());
            }
            String logName = vizEnvironment.getLogDirectory() + 
                Path.DirectorySeparatorChar + this.constructLogName(this.determineLogDate());

            // Prepare the process.
            this.process = new Process();
            this.process.StartInfo = this.constructProcessStartInfo(vizEnvironment);
            this.process.OutputDataReceived += new DataReceivedEventHandler(processOutputHandler);
            this.validate();
            if (this.ready == false)
            {
                return;
            }

            /*
             * Access the log file for write access; other processes will have read-only access to
             * the log file until it is closed.
             **/
            this.logFileWriter =
                new StreamWriter(File.Open(logName, FileMode.Append, 
                FileAccess.Write, FileShare.Read));
        }

        private String determineLogDate()
        {
            return DateTime.Now.ToString(LOG_DATE_FORMAT);
        }

        private ProcessStartInfo constructProcessStartInfo(VizEnvironment vizEnvironment)
        {
            ProcessStartInfo processStartInfo = 
                new ProcessStartInfo(this.constructProcessName(vizEnvironment.getLocation()));
            // include the default system PATH in the application PATH
            String systemPath = System.Environment.GetEnvironmentVariable("PATH");
            processStartInfo.EnvironmentVariables[EnvironmentProperties.ENVIRONMENT_VARIABLE_PATH] =
                vizEnvironment.getPath() + this.getApplicationSpecificPath() + systemPath;
            processStartInfo.EnvironmentVariables[EnvironmentProperties.ENVIRONMENT_VARIABLE_PYTHON_PATH] =
                vizEnvironment.getPythonPath();
            processStartInfo.UseShellExecute = false;
            processStartInfo.Arguments = this.getCommandLineArguments();
            processStartInfo.RedirectStandardOutput = true;

            return processStartInfo;
        }

        protected void validate()
        {
            String application = this.process.StartInfo.FileName;
            /* ensure that the specified application exists. */
            if (File.Exists(application) == false)
            {
                this.ready = false;
                this.exceptionText = "Unable to find the specified Viz application: " + application;
                return;
            }

            this.ready = true;
        }

        public virtual void launchProcess()
        {
            this.runProcess();
            this.closeLog();
        }

        protected void runProcess()
        {
            this.process.Start();
            this.process.BeginOutputReadLine();
            this.process.WaitForExit();
            this.process.CancelOutputRead();
        }

        protected void closeLog()
        {
            this.logFileWriter.Close();
        }

        private void processOutputHandler(Object sendingProcess, DataReceivedEventArgs outline)
        {
            if (String.IsNullOrEmpty(outline.Data))
            {
                return;
            }
            this.logFileWriter.WriteLine(outline.Data);
        }

        public bool isReady()
        {
            return this.ready;
        }

        public String getExceptionText()
        {
            return this.exceptionText;
        }

        protected virtual String getApplicationSpecificPath()
        {
            return String.Empty;
        }

        protected abstract String constructProcessName(String location);

        protected abstract String constructLogName(String logDate);

        protected abstract String getCommandLineArguments();
    }
}