/*
 * Win32Service.java
 *
 * Created on 12. September 2007, 12:05
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package jnacontrib.win32;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import jnacontrib.jna.Advapi32;
import jnacontrib.jna.WINERROR;
import jnacontrib.jna.WINNT;
import jnacontrib.jna.WINSVC;
import jnacontrib.jna.Advapi32.ENUM_SERVICE_STATUS_PROCESS;
import jnacontrib.jna.Advapi32.QUERY_SERVICE_CONFIG;
import jnacontrib.jna.Advapi32.SERVICE_DESCRIPTION;
import jnacontrib.jna.Advapi32.SERVICE_FAILURE_ACTIONS;
import jnacontrib.jna.Advapi32.SERVICE_STATUS_PROCESS;

import org.rzo.yajsw.os.Service;
import org.rzo.yajsw.os.ServiceInfo;
import org.rzo.yajsw.os.ServiceInfoImpl;
import org.rzo.yajsw.util.MyReentrantLock;

import com.sun.jna.Memory;
import com.sun.jna.Native;
import com.sun.jna.PlatformEx;
import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.Kernel32Util;
import com.sun.jna.ptr.IntByReference;

/**
 * Baseclass for a Win32 service.
 */
abstract public class Win32Service
{
	protected String		serviceName;
	private ServiceMain		serviceMain;
	private ServiceControl	serviceControl;
	private Pointer			serviceStatusHandle;
	protected Object		waitObject			= new Object();
	private int				stopTimeout			= 5000;
	private int				startupTimeout		= 30000;
	protected volatile int	checkPoint			= 0;
	private boolean			autoReportStartup	= true;
	private Lock			startupLock			= new MyReentrantLock();
	private Condition		startupCondition	= startupLock.newCondition();
	private volatile boolean _stopping = false;
	public volatile String _stopReason = null;
	private static int 			lastWinError = -1;
	protected boolean _debug = false;


	/**
	 * Creates a new instance of Win32Service.
	 * 
	 * @param serviceName
	 *            internal name of the service
	 */
	public Win32Service(String serviceName)
	{
		this.serviceName = serviceName;
	}

	public Win32Service()
	{

	}

	public void setServiceName(String serviceName)
	{
		this.serviceName = serviceName;
	}

	public String getServiceName()
	{
		return serviceName;
	}
	
	public void setDebug(boolean value)
	{
		_debug = value;
	}

	/**
	 * Install the service.
	 * 
	 * @param displayName
	 *            visible name
	 * @param description
	 *            description
	 * @param dependencies
	 *            array of other services to depend on or null
	 * @param account
	 *            service account or null for LocalSystem
	 * @param password
	 *            password for service account or null
	 * @throws java.lang.Exception
	 * @return true on success
	 */
	public boolean install(String displayName, String description, String[] dependencies, String account, String password, boolean delayedAutostart)
	{
		return (install(displayName, description, dependencies, account, password, "java.exe -cp \"" + System.getProperty("java.class.path")
				+ "\" -Xrs " + this.getClass().getName(), "AUTO_START", false, null));
	}

	/**
	 * Install the service.
	 * 
	 * @return true on success
	 * @param displayName
	 *            visible name
	 * @param description
	 *            description
	 * @param dependencies
	 *            array of other services to depend on or null
	 * @param account
	 *            service account or null for LocalSystem
	 * @param password
	 *            password for service account or null
	 * @param command
	 *            command line to start the service
	 * @throws java.lang.Exception
	 */
	public boolean install(String displayName, String description, String[] dependencies, String account, String password, String command,
			String startType, boolean interactive, Object failureActions)
	{
		Advapi32 advapi32;
		Advapi32.SERVICE_DESCRIPTION desc;
		Pointer serviceManager, service;
		boolean success = false;
		String dep = "";

		if (dependencies != null)
		{
			for (String s : dependencies)
			{
				dep += s + "\0";
			}
		}
		dep += "\0";

		desc = new Advapi32.SERVICE_DESCRIPTION();
		desc.lpDescription = description;

		advapi32 = Advapi32.INSTANCE;
		serviceManager = openServiceControlManager(null, WINSVC.SC_MANAGER_ALL_ACCESS);

		int winStartType = "DEMAND_START".equals(startType) ? WINSVC.SERVICE_DEMAND_START : WINSVC.SERVICE_AUTO_START;
		int dwServiceType = WINSVC.SERVICE_WIN32_OWN_PROCESS;
		if (interactive)
			dwServiceType |= WINSVC.SERVICE_INTERACTIVE_PROCESS;
		if (serviceManager != null)
		{
			System.out.println("service cmd: "+command);
			service = advapi32.CreateService(serviceManager, serviceName, displayName, WINSVC.SERVICE_ALL_ACCESS, dwServiceType, winStartType,
					WINSVC.SERVICE_ERROR_NORMAL, command, null, null, dep, account, password);

			if (service != null)
			{
				if (failureActions != null)
				{
					success = advapi32.ChangeServiceConfig2(service, WINSVC.SERVICE_CONFIG_FAILURE_ACTIONS, (SERVICE_FAILURE_ACTIONS)failureActions);
					if (!success)
					{
						int err = Native.getLastError();
						System.out.println("ERROR Setting failure actions #"+err+ " "+Kernel32Util.formatMessageFromLastErrorCode(err));
					}
					
				}
				success = advapi32.ChangeServiceConfig2(service, WINSVC.SERVICE_CONFIG_DESCRIPTION, desc);
				if (PlatformEx.isWinVista() && "DELAYED_AUTO_START".equals(startType))
				{
					Advapi32.SERVICE_DELAYED_AUTO_START_INFO delayedDesc = new Advapi32.SERVICE_DELAYED_AUTO_START_INFO();
					delayedDesc.fDelayedAutostart = true;
					success = advapi32.ChangeServiceConfig2(service, WINSVC.SERVICE_CONFIG_DELAYED_AUTO_START_INFO, delayedDesc);					
				}
				advapi32.CloseServiceHandle(service);
			}
			else
			{
				int err = Kernel32.INSTANCE.GetLastError();
				System.out.println("error during install " + err);
				System.out.println(Kernel32Util.formatMessageFromLastErrorCode(err));
			}

			advapi32.CloseServiceHandle(serviceManager);
		}
		return (success);
	}

	/**
	 * Uninstall the service.
	 * 
	 * @throws java.lang.Exception
	 * @return true on success
	 */
	public boolean uninstall()
	{
		Advapi32 advapi32;
		Pointer serviceManager, service;
		boolean success = false;

		advapi32 = Advapi32.INSTANCE;
		serviceManager = openServiceControlManager(null, WINSVC.SC_MANAGER_ALL_ACCESS);

		if (serviceManager != null)
		{
			service = advapi32.OpenService(serviceManager, serviceName, WINSVC.SERVICE_ALL_ACCESS);

			if (service != null)
			{
				success = advapi32.DeleteService(service);
				advapi32.CloseServiceHandle(service);
			}
			advapi32.CloseServiceHandle(serviceManager);
		}
		return (success);
	}

	public static ServiceInfo serviceInfo(String name)
	{
		ServiceInfoImpl result = new ServiceInfoImpl();
		result.setName(name);

		Advapi32 advapi32;
		Pointer serviceManager, service;
		int state = Service.STATE_UNKNOWN;

		advapi32 = Advapi32.INSTANCE;

		serviceManager = openServiceControlManager(null, WINNT.GENERIC_READ);

		if (serviceManager != null)
		{
			state = 0;
			service = advapi32.OpenService(serviceManager, name, WINNT.GENERIC_READ);

			if (service != null)
			{
				IntByReference pcbBytesNeeded = new IntByReference();
				state |= Service.STATE_INSTALLED;
				// get size required
				if (!advapi32.QueryServiceConfig(service, null, 0, pcbBytesNeeded))
				{
					// now get the data
					int cbBufSize = pcbBytesNeeded.getValue();
					Memory buffer = new Memory(cbBufSize);
					buffer.clear();
					if (advapi32.QueryServiceConfig(service, buffer, cbBufSize, pcbBytesNeeded))
					{
						QUERY_SERVICE_CONFIG lpServiceConfig = new QUERY_SERVICE_CONFIG();
						lpServiceConfig.init(buffer);
						if (lpServiceConfig.dwStartType == Advapi32.SERVICE_DISABLED)
							state |= Service.STATE_DISABLED;
						if (lpServiceConfig.dwStartType == Advapi32.SERVICE_BOOT_START | lpServiceConfig.dwStartType == Advapi32.SERVICE_SYSTEM_START
								| lpServiceConfig.dwStartType == Advapi32.SERVICE_AUTO_START)
							state |= Service.STATE_AUTOMATIC;
						if (lpServiceConfig.dwStartType == Advapi32.SERVICE_DEMAND_START)
							state |= Service.STATE_MANUAL;
						if ((lpServiceConfig.dwServiceType & Advapi32.SERVICE_INTERACTIVE_PROCESS) != 0)
							state |= Service.STATE_INTERACTIVE;
						result.setAccount(lpServiceConfig.lpServiceStartName);
						result.setCommand(lpServiceConfig.lpBinaryPathName);
						result.setDependencies(lpServiceConfig.getDependencies());
						result.setDisplayName(lpServiceConfig.lpDisplayName);

					}
					else
					{
						state |= Service.STATE_UNKNOWN;
						System.out.println("Error in QueryServiceConfig: " + Native.getLastError());
					}
				}
				else
				{
					state |= Service.STATE_UNKNOWN;
					System.out.println("Error in QueryServiceConfig: " + Native.getLastError());
				}
				if (!advapi32.QueryServiceStatusEx(service, (byte) advapi32.SC_STATUS_PROCESS_INFO, null, 0, pcbBytesNeeded))
				{
					// now get the data
					int cbBufSize = pcbBytesNeeded.getValue();
					Memory buffer = new Memory(cbBufSize);
					buffer.clear();
					if (advapi32.QueryServiceStatusEx(service, (byte) advapi32.SC_STATUS_PROCESS_INFO, buffer, cbBufSize, pcbBytesNeeded))
					{
						SERVICE_STATUS_PROCESS lpBuffer = new SERVICE_STATUS_PROCESS();
						lpBuffer.init(buffer);
						if (lpBuffer.dwCurrentState == advapi32.SERVICE_RUNNING)
							state |= Service.STATE_RUNNING;
						if (lpBuffer.dwCurrentState == advapi32.SERVICE_PAUSED)
							state |= Service.STATE_PAUSED;
						if (lpBuffer.dwCurrentState == advapi32.SERVICE_START_PENDING)
							state |= Service.STATE_STARTING;
						if (lpBuffer.dwCurrentState == advapi32.SERVICE_STOP_PENDING)
							state |= Service.STATE_STOPPING;
						result.setPid(lpBuffer.dwProcessId);
					}
					else
					{
						state |= Service.STATE_UNKNOWN;
						System.out.println("Error in QueryServiceStatusEx: " + Native.getLastError());
					}
				}
				if (!advapi32.QueryServiceConfig2(service, (byte) advapi32.SERVICE_CONFIG_DESCRIPTION, null, 0, pcbBytesNeeded))
				{
					// now get the data
					int cbBufSize = pcbBytesNeeded.getValue();
					Memory buffer = new Memory(cbBufSize);
					buffer.clear();
					if (advapi32.QueryServiceConfig2(service, (byte) advapi32.SERVICE_CONFIG_DESCRIPTION, buffer, cbBufSize, pcbBytesNeeded))
					{
						SERVICE_DESCRIPTION lpBuffer = new SERVICE_DESCRIPTION();
						lpBuffer.init(buffer);
						result.setDescription(lpBuffer.lpDescription);
					}
					else
					{
						state |= Service.STATE_UNKNOWN;
						System.out.println("Error in QueryServiceStatusEx: " + Native.getLastError());
					}
				}

				else
				{
					state |= Service.STATE_UNKNOWN;
					System.out.println("Error in QueryServiceStatusEx: " + Native.getLastError());
				}

				advapi32.CloseServiceHandle(service);
			}
			advapi32.CloseServiceHandle(serviceManager);
		}
		result.setState(state);

		return result;

	}

	public int state()
	{
		Advapi32 advapi32;
		Pointer serviceManager, service;
		int result = Service.STATE_UNKNOWN;

		advapi32 = Advapi32.INSTANCE;

		serviceManager = openServiceControlManager(null, WINNT.GENERIC_READ);
		// System.out.println("Win32Service.state() serviceManager "+serviceManager);

		if (serviceManager != null)
		{
			result = 0;
			service = advapi32.OpenService(serviceManager, serviceName, WINNT.GENERIC_READ);
			// System.out.println("Win32Service.state() service "+service);

			if (service != null)
			{
				IntByReference pcbBytesNeeded = new IntByReference();
				result |= Service.STATE_INSTALLED;
				// get size required
				if (!advapi32.QueryServiceConfig(service, null, 0, pcbBytesNeeded))
				{
					// now get the data
					int cbBufSize = pcbBytesNeeded.getValue();
					if (cbBufSize > 8192)
						cbBufSize = 8192;
					Memory buffer = new Memory(cbBufSize);
					buffer.clear();
					if (advapi32.QueryServiceConfig(service, buffer, cbBufSize, pcbBytesNeeded))
					{
						QUERY_SERVICE_CONFIG lpServiceConfig = new QUERY_SERVICE_CONFIG();
						lpServiceConfig.init(buffer);
						if (lpServiceConfig.dwStartType == Advapi32.SERVICE_DISABLED)
							result |= Service.STATE_DISABLED;
						if (lpServiceConfig.dwStartType == Advapi32.SERVICE_BOOT_START | lpServiceConfig.dwStartType == Advapi32.SERVICE_SYSTEM_START
								| lpServiceConfig.dwStartType == Advapi32.SERVICE_AUTO_START)
							result |= Service.STATE_AUTOMATIC;
						if (lpServiceConfig.dwStartType == Advapi32.SERVICE_DEMAND_START)
							result |= Service.STATE_MANUAL;
						if ((lpServiceConfig.dwServiceType & Advapi32.SERVICE_INTERACTIVE_PROCESS) != 0)
							result |= Service.STATE_INTERACTIVE;

					}
					else
					{
						result |= Service.STATE_UNKNOWN;
						int error = Native.getLastError();
						System.out.println("Error getting buffer size in QueryServiceConfig: " + error + " " + Kernel32Util.formatMessageFromLastErrorCode(error));
					}
				}
				else
				{
					result |= Service.STATE_UNKNOWN;
					int error = Native.getLastError();
					System.out.println("Error in QueryServiceConfig: " + error + " " + Kernel32Util.formatMessageFromLastErrorCode(error));
				}
				if (!advapi32.QueryServiceStatusEx(service, (byte) advapi32.SC_STATUS_PROCESS_INFO, null, 0, pcbBytesNeeded))
				{
					// now get the data
					int cbBufSize = pcbBytesNeeded.getValue();
					Memory buffer = new Memory(cbBufSize);
					buffer.clear();
					if (advapi32.QueryServiceStatusEx(service, (byte) advapi32.SC_STATUS_PROCESS_INFO, buffer, cbBufSize, pcbBytesNeeded))
					{
						SERVICE_STATUS_PROCESS lpBuffer = new SERVICE_STATUS_PROCESS();
						lpBuffer.init(buffer);
						if (lpBuffer.dwCurrentState == advapi32.SERVICE_RUNNING)
							result |= Service.STATE_RUNNING;
						if (lpBuffer.dwCurrentState == advapi32.SERVICE_PAUSED)
							result |= Service.STATE_PAUSED;
						// System.out.println("Win32Service.state() dwCurrentState "+lpBuffer.dwCurrentState);

					}
					else
					{
						result |= Service.STATE_UNKNOWN;
						int error = Native.getLastError();
						System.out.println("Error getting buffer size in QueryServiceStatusEx: " + error + " " + Kernel32Util.formatMessageFromLastErrorCode(error));
					}
				}
				else
				{
					result |= Service.STATE_UNKNOWN;
					int error = Native.getLastError();
					System.out.println("Error in QueryServiceStatusEx: " + error + " " + Kernel32Util.formatMessageFromLastErrorCode(error));
				}

				advapi32.CloseServiceHandle(service);
			}
			advapi32.CloseServiceHandle(serviceManager);
		}

		return result;

	}

	/**
	 * Ask the ServiceControlManager to start the service.
	 * 
	 * @return true on success
	 */
	public boolean start()
	{
		Advapi32 advapi32;
		Pointer serviceManager, service;
		boolean success = false;

		advapi32 = Advapi32.INSTANCE;

		serviceManager = openServiceControlManager(null, WINNT.GENERIC_EXECUTE);
		// System.out.println("service.start() serviceManager "+serviceManager);

		if (serviceManager != null)
		{
			service = advapi32.OpenService(serviceManager, serviceName, WINNT.GENERIC_EXECUTE);
			// System.out.println("service.start() service "+service);

			if (service != null)
			{
				success = advapi32.StartService(service, 0, null);
				// System.out.println("service.start() StartService "+success);
				advapi32.CloseServiceHandle(service);
			}
			advapi32.CloseServiceHandle(serviceManager);
		}

		return (success);
	}

	/**
	 * Ask the ServiceControlManager to stop the service.
	 * 
	 * @return true on success
	 */
	public boolean stop() throws Exception
	{
		Advapi32 advapi32;
		Pointer serviceManager, service;
		Advapi32.SERVICE_STATUS serviceStatus;
		boolean success = false;

		advapi32 = Advapi32.INSTANCE;

		serviceManager = openServiceControlManager(null, WINNT.GENERIC_EXECUTE);

		if (serviceManager != null)
		{
			service = advapi32.OpenService(serviceManager, serviceName, WINNT.GENERIC_EXECUTE);

			if (service != null)
			{
				serviceStatus = new Advapi32.SERVICE_STATUS();
				success = advapi32.ControlService(service, WINSVC.SERVICE_CONTROL_STOP, serviceStatus);
				advapi32.CloseServiceHandle(service);
			}
			advapi32.CloseServiceHandle(serviceManager);
		}

		return (success);
	}

	/**
	 * Initialize the service, connect to the ServiceControlManager.
	 */
	public void init()
	{
		Advapi32 advapi32;
		// Advapi32.SERVICE_TABLE_ENTRY[] entries = new
		// Advapi32.SERVICE_TABLE_ENTRY[2];
		Advapi32.SERVICE_TABLE_ENTRY entry;

		serviceMain = new ServiceMain();
		advapi32 = Advapi32.INSTANCE;
		entry = new Advapi32.SERVICE_TABLE_ENTRY();
		entry.size();
		entry.lpServiceName = serviceName;
		entry.lpServiceProc = serviceMain;
		entry.write();

		if (!advapi32.StartServiceCtrlDispatcher(entry.toArray(2)))
		{
			log("error in StartServiceCtrlDispatcher");
			int err = Native.getLastError();
			lastWinError = err;
			log(err + ":" + Kernel32Util.formatMessageFromLastErrorCode(err));
		}
	}

	public void setStopTimeout(int t)
	{
		stopTimeout = t;
	}

	public int getStopTimeout()
	{
		return stopTimeout;
	}

	public void reportStartup()
	{
		reportStatus(WINSVC.SERVICE_RUNNING, WINERROR.NO_ERROR, 0);

		onStart();

		try
		{
			synchronized (waitObject)
			{
				waitObject.wait();
			}
		}
		catch (InterruptedException ex)
		{
		}
		reportStatus(WINSVC.SERVICE_STOPPED, WINERROR.NO_ERROR, 0);

		// Avoid returning from ServiceMain, which will cause a crash
		// See http://support.microsoft.com/kb/201349, which recommends
		// having init() wait for this thread.
		// Waiting on this thread in init() won't fix the crash, though.
		// System.exit(0);

	}

	/**
	 * Get a handle to the ServiceControlManager.
	 * 
	 * @param machine
	 *            name of the machine or null for localhost
	 * @param access
	 *            access flags
	 * @return handle to ServiceControlManager or null when failed
	 */
	static private Pointer openServiceControlManager(String machine, int access)
	{
		Pointer handle = null;
		Advapi32 advapi32;

		advapi32 = Advapi32.INSTANCE;
		handle = advapi32.OpenSCManager(machine, null, access);
		if (handle == null)
		{
			int err = Native.getLastError();
			lastWinError = err;
			System.out.println("Error in OpenSCManager: " + Integer.toHexString(err));
			if (err == 5)
				System.out.println("Access denied: please check the user credentials");
		}

		return (handle);
	}

	static public Map<String, ENUM_SERVICE_STATUS_PROCESS> enumerateServices(String machine)
	{
		Map<String, ENUM_SERVICE_STATUS_PROCESS> result = new HashMap();
		// Open the Service Control Manager
		Pointer sc = openServiceControlManager(machine, WINSVC.SC_MANAGER_ENUMERATE_SERVICE);

		// Check if OpenSCManager returns NULL. Otherwise proceed
		if (sc != null && !sc.equals(null))
		{
			Memory service_data = null;
			int service_data_size = 0;
			int infoLevel = WINSVC.SC_ENUM_PROCESS_INFO;
			boolean retVal;
			IntByReference bytesNeeded = new IntByReference(0);
			IntByReference srvCount = new IntByReference(0);
			IntByReference resumeHandle = new IntByReference(0);
			int srvType = WINSVC.SERVICE_WIN32;
			int srvState = WINSVC.SERVICE_STATE_ALL;

			// Call EnumServicesStatus with null data and data_size == 0, so we
			// get the required memory size
			retVal = Advapi32.INSTANCE.EnumServicesStatusExW(sc, infoLevel, srvType, srvState, service_data, service_data_size, bytesNeeded,
					srvCount, resumeHandle, null);

			int err = Native.getLastError();
			// EnumServicesStatus should need more memory space
			if ((!retVal) || err == WINERROR.ERROR_MORE_DATA)
			{
				int bytesCount = bytesNeeded.getValue();
				service_data = new Memory(bytesCount);
				service_data.clear();
				service_data_size = bytesCount;
				// System.out.println(resumeHandle.getValue());
				resumeHandle.setValue(0);
				retVal = Advapi32.INSTANCE.EnumServicesStatusExW(sc, infoLevel, srvType, srvState, service_data, service_data_size, bytesNeeded,
						srvCount, resumeHandle, null);
				if (!retVal)
				{
					err = Native.getLastError();
					System.out.println("Error in EnumServicesStatusExA " + Integer.toHexString(err));
					return null;
				}
			}
			else
				return null;

			ENUM_SERVICE_STATUS_PROCESS serviceStatus = new ENUM_SERVICE_STATUS_PROCESS();
			serviceStatus.init(service_data);
			for (int i = 0; i < srvCount.getValue(); i++)
			{
				result.put(serviceStatus.getServiceName().toLowerCase(), serviceStatus);
				serviceStatus = serviceStatus.next();
			}
		}

		// Close the SC_HANLDE returned by OpenSCManager
		Advapi32.INSTANCE.CloseServiceHandle(sc);

		return result;
	}

	/**
	 * Report service status to the ServiceControlManager.
	 * 
	 * @param status
	 *            status
	 * @param win32ExitCode
	 *            exit code
	 * @param waitHint
	 *            time to wait
	 */
	protected void reportStatus(int status, int win32ExitCode, int waitHint)
	{
		Advapi32 advapi32;
		Advapi32.SERVICE_STATUS serviceStatus;

		advapi32 = Advapi32.INSTANCE;
		serviceStatus = new Advapi32.SERVICE_STATUS();
		serviceStatus.dwServiceType = WINNT.SERVICE_WIN32_OWN_PROCESS;
		serviceStatus.dwControlsAccepted = WINSVC.SERVICE_ACCEPT_STOP | WINSVC.SERVICE_ACCEPT_SHUTDOWN;
		serviceStatus.dwWin32ExitCode = win32ExitCode;
		serviceStatus.dwWaitHint = waitHint;
		serviceStatus.dwCurrentState = status;
		serviceStatus.dwCheckPoint = checkPoint;
		log("reporting status " + checkPoint);

		advapi32.SetServiceStatus(serviceStatusHandle, serviceStatus);
	}

	/**
	 * Called when service is starting.
	 */
	public abstract void onStart();

	/*
	 * Called when service should stop.
	 */
	public abstract void onStop();

	public abstract void log(String txt);

	/**
	 * Implementation of the service main function.
	 */
	private class ServiceMain implements Advapi32.SERVICE_MAIN_FUNCTION
	{

		/**
		 * Called when the service is starting.
		 * 
		 * @param dwArgc
		 *            number of arguments
		 * @param lpszArgv
		 *            pointer to arguments
		 */
		public void callback(int dwArgc, Pointer lpszArgv)
		{
			Advapi32 advapi32;

			advapi32 = Advapi32.INSTANCE;

			log("+ ServiceMain callback");
			serviceControl = new ServiceControl();
			serviceStatusHandle = advapi32.RegisterServiceCtrlHandlerEx(serviceName, serviceControl, null);

			// if we are waiting for application to report startup
			if (!autoReportStartup)
				try
				{
					// report the startup time
					reportStatus(WINSVC.SERVICE_START_PENDING, WINERROR.NO_ERROR, startupTimeout);
					// wait for application to send startup notification
					startupLock.lock();
					if (!startupCondition.await(startupTimeout, TimeUnit.MILLISECONDS))
					{
						log("service startup timeout -> aborting");
						System.exit(999);
					}
				}
				catch (InterruptedException e)
				{
					e.printStackTrace();
				}
				finally
				{
					startupLock.unlock();
				}
			else
				// if we are not waiting for the application, give us at most 5
				// seconds to report startup
				reportStatus(WINSVC.SERVICE_START_PENDING, WINERROR.NO_ERROR, 5000);

			// this method will hang until the service terminates
			reportStartup();

		}
	}

	/**
	 * Implementation of the service control function.
	 */
	private class ServiceControl implements Advapi32.HandlerEx
	{

		/**
		 * Called when the service get a control code.
		 * 
		 * @param dwControl
		 * @param dwEventType
		 * @param lpEventData
		 * @param lpContext
		 */
		public int callback(int dwControl, int dwEventType, Pointer lpEventData, Pointer lpContext)
		{
			log("received service control " + dwControl);
			switch (dwControl)
			{
			case WINSVC.SERVICE_CONTROL_STOP:
			case WINSVC.SERVICE_CONTROL_SHUTDOWN:
				checkPoint = 1;
				reportStatus(WINSVC.SERVICE_STOP_PENDING, WINERROR.NO_ERROR, stopTimeout);
				_stopping = true;
				if (dwControl == WINSVC.SERVICE_CONTROL_STOP)
					_stopReason = "SERVICE";
				if (dwControl == WINSVC.SERVICE_CONTROL_SHUTDOWN)
					_stopReason = "COMPUTER";
				onStop();
			}
			return WINERROR.NO_ERROR;
		}
	}

	public int getStartupTimeout()
	{
		return startupTimeout;
	}

	public boolean isAutoReportStartup()
	{
		return autoReportStartup;
	}

	public void setStartupTimeout(int startupTimeout)
	{
		this.startupTimeout = startupTimeout;
	}

	public void setAutoReportStartup(boolean autoReportStartup)
	{
		this.autoReportStartup = autoReportStartup;
	}

	public void notifyStartup()
	{
		try
		{
			startupLock.lock();
			startupCondition.signal();
		}
		finally
		{
			startupLock.unlock();
		}
	}
	
	public void signalStopping(long waitHint)
	{
		if (_stopping)
			reportStatus(WINSVC.SERVICE_STOP_PENDING, WINERROR.NO_ERROR, (int)waitHint);
	}
	
	public boolean requestElevation()
	{
		return lastWinError == 5;
	}

}
