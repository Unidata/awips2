package org.rzo.yajsw.updater;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.configuration.FileOptionsProvider;
import org.apache.commons.configuration.FileSystem;
import org.apache.commons.configuration.VFSFileSystem;
import org.apache.commons.vfs2.FileObject;
import org.apache.commons.vfs2.FileSelectInfo;
import org.apache.commons.vfs2.FileSelector;
import org.apache.commons.vfs2.FileSystemException;
import org.rzo.yajsw.boot.WrapperLoader;
import org.rzo.yajsw.config.YajswConfigurationImpl;
import org.rzo.yajsw.os.ms.win.w32.WindowsXPProcess;
import org.rzo.yajsw.util.CommonsLoggingAdapter;
import org.rzo.yajsw.util.VFSUtils;
import org.rzo.yajsw.wrapper.WrappedService;

import com.sun.jna.PlatformEx;

public class UpdaterMain
{
	// args: update.conf currentApp.conf
	public static void main(String[] args) throws Exception
	{
		if (args.length < 2)
		{
			System.err.println("missing update or current configuration -> abort");
			return;
		}
		System.setProperty("wrapper.config", args[0]);
		YajswConfigurationImpl updateConfig = new YajswConfigurationImpl();
		
		System.setProperty("wrapper.config", args[1]);
		YajswConfigurationImpl currentAppConfig = new YajswConfigurationImpl();
		
		// first uninstall service so it cannot be restarted
		uninstallService(args[1]);
		
		String wrapperHome = updateWrapper(updateConfig);

		// spawn installer process with updated wrapper in classpath and current and new configurations
		InstallAction.setCurrentConfig(currentAppConfig);
		InstallAction.setNewConfig(updateConfig.getString("update.app.config"));
		InstallAction.setWrapperHome(wrapperHome);
		InstallAction.run();

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
	private static String updateWrapper(YajswConfigurationImpl updateConfig) throws Exception
	{
		String wrapperSrcFile = updateConfig.getString("update.wrapper.src", null);
		String wrapperDestFile = updateConfig.getString("update.wrapper.dest", null);
		if (wrapperSrcFile == null && wrapperDestFile == null)
			return WrapperLoader.getWrapperHome();
		if (wrapperSrcFile == null || wrapperDestFile == null)
		{
			System.out.println("wrapper src or destination is empty -> not updating wrapper");
			return WrapperLoader.getWrapperHome();
		}

		FileObject wrapperSrc = VFSUtils.resolveFile((String)null, wrapperSrcFile);
		FileObject wrapperDest = VFSUtils.resolveFile((String)null, wrapperDestFile);
		wrapperDest.copyFrom(wrapperSrc, new FileSelector()
		{

			public boolean includeFile(FileSelectInfo arg0) throws Exception
			{
				return true;
			}

			public boolean traverseDescendents(FileSelectInfo arg0) throws Exception
			{
				return true;
			}
			
		});
		return wrapperDest.getName().getPath();
		
	}


}
