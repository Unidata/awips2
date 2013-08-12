using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows.Forms;
using VizLauncher.com.raytheon.viz.launcher;

namespace VizLauncher
{
    static class Program
    {
        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        static void Main()
        {
            VizLauncher.com.raytheon.viz.launcher.VizLauncher vizLauncher = 
                new VizLauncher.com.raytheon.viz.launcher.VizLauncher();
            bool success = vizLauncher.run(Application.StartupPath);
            if (success == false)
            {
                // Display the "Failure" dialog.
                Application.EnableVisualStyles();
                Application.SetCompatibleTextRenderingDefault(false);
                Application.Run(new Form1(vizLauncher.getErrorDetail()));
            }

            Application.Exit();
        }
    }
}
