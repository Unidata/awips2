using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Diagnostics;

namespace VizLauncher.com.raytheon.viz.launcher.process
{
    public interface IProcessLauncher
    {
        void launchProcess();

        bool isReady();
        String getExceptionText();
    }
}