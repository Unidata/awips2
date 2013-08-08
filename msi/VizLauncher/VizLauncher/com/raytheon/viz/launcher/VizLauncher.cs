using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using VizLauncher.com.raytheon.viz.launcher.environment;
using VizLauncher.com.raytheon.viz.launcher.process;
using VizLauncher.com.raytheon.viz.launcher.process.impl;

namespace VizLauncher.com.raytheon.viz.launcher
{
    public class VizLauncher
    {
        private String errorDetail;

        public bool run(String location)
        {
            VizEnvironment vizEnvironment = new VizEnvironment(location);
            if (vizEnvironment.isReady() == false)
            {
                this.errorDetail = vizEnvironment.getExceptionText();
                return false;
            }

            /* Alternatively, we would be able to construct both process launchers using Spring and inject them. */

            // Construct the AlertViz Process Launcher.
            IProcessLauncher alertvizProcessLauncher = new AlertvizProcessLauncher(vizEnvironment);
            if (alertvizProcessLauncher.isReady() == false)
            {
                this.errorDetail = alertvizProcessLauncher.getExceptionText();
                return false;
            }
            Thread alertvizThread = new Thread(alertvizProcessLauncher.launchProcess);

            // Construct the CAVE Process Launcher.
            IProcessLauncher caveProcessLauncher = new CaveProcessLauncher(vizEnvironment);
            if (caveProcessLauncher.isReady() == false)
            {
                this.errorDetail = caveProcessLauncher.getExceptionText();
                return false;
            }
            Thread caveThread = new Thread(caveProcessLauncher.launchProcess);

            // Start AlertViz.
            alertvizThread.Start();

            // Delay - Give Alertviz Time To Start.
            Thread.Sleep(1000);

            // Start CAVE.
            caveThread.Start();

            // Wait for CAVE.
            caveThread.Join();

            // Wait for AlertViz.
            alertvizThread.Join();

            return true;
        }

        public String getErrorDetail()
        {
            return this.errorDetail;
        }
    }
}