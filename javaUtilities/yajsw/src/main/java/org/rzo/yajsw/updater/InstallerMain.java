package org.rzo.yajsw.updater;

import java.util.ArrayList;

import org.rzo.yajsw.config.YajswConfigurationImpl;
import org.rzo.yajsw.os.ms.win.w32.WindowsXPProcess;
import org.rzo.yajsw.wrapper.WrappedService;

import com.sun.jna.PlatformEx;

public class InstallerMain
{

	// args: currentApp.conf newApp.conf
	public static void main(String[] args) throws Exception
	{
		if (args.length < 2)
		{
			System.err.println("missing new or current configuration -> abort");
			return;
		}
		uninstallService(args[0]);
		try
		{
			Thread.sleep(2000);
		}
		catch (InterruptedException e)
		{
			e.printStackTrace();
			Thread.currentThread().interrupt();
		}
		installService(args[1]);
	}

	
	private static void installService(String newAppConfig)
	{
		WrappedService service = new WrappedService();
		ArrayList<String> list = new ArrayList<String>();
		list.add(newAppConfig);
		service.setConfFilesList(list);
		service.init();
		
		boolean result = service.install();
		if (PlatformEx.isWinVista() && service.requiresElevate())
		{
			System.out.println("try uac elevate");
			WindowsXPProcess.elevateMe();
			return;
		}

		if (result && System.getProperty("update.autostart") != null)
			service.start();
		else
			System.out.println("error installing");
	}

	private static void uninstallService(String currentConfig)
	{
		WrappedService service = new WrappedService();
		ArrayList<String> list = new ArrayList<String>();
		list.add(currentConfig);
		service.init();
		service.stop();
		service.uninstall();
	}


}
