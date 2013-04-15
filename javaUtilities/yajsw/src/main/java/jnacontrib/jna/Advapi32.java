/*
 * Advapi32.java
 *
 * Created on 6. August 2007, 11:24
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package jnacontrib.jna;

import java.util.ArrayList;
import java.util.List;

import com.sun.jna.Memory;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import com.sun.jna.WString;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.ptr.PointerByReference;
import com.sun.jna.win32.StdCallLibrary;

/**
 * 
 * @author TB
 */
public interface Advapi32 extends StdCallLibrary
{
	Advapi32	INSTANCE	= (Advapi32) Native.loadLibrary("Advapi32", Advapi32.class, Options.UNICODE_OPTIONS);

	/*
	 * BOOL WINAPI LookupAccountName( LPCTSTR lpSystemName, LPCTSTR
	 * lpAccountName, PSID Sid, LPDWORD cbSid, LPTSTR ReferencedDomainName,
	 * LPDWORD cchReferencedDomainName, PSID_NAME_USE peUse );
	 */
	public boolean LookupAccountName(String lpSystemName, String lpAccountName, byte[] Sid, IntByReference cbSid, char[] ReferencedDomainName,
			IntByReference cchReferencedDomainName, PointerByReference peUse);

	/*
	 * BOOL WINAPI LookupAccountSid( LPCTSTR lpSystemName, PSID lpSid, LPTSTR
	 * lpName, LPDWORD cchName, LPTSTR lpReferencedDomainName, LPDWORD
	 * cchReferencedDomainName, PSID_NAME_USE peUse );
	 */
	public boolean LookupAccountSid(String lpSystemName, byte[] Sid, char[] lpName, IntByReference cchName, char[] ReferencedDomainName,
			IntByReference cchReferencedDomainName, PointerByReference peUse);

	/*
	 * BOOL ConvertSidToStringSid( PSID Sid, LPTSTR* StringSid );
	 */
	public boolean ConvertSidToStringSid(byte[] Sid, PointerByReference StringSid);

	/*
	 * BOOL WINAPI ConvertStringSidToSid( LPCTSTR StringSid, PSID* Sid );
	 */
	public boolean ConvertStringSidToSid(String StringSid, PointerByReference Sid);

	/*
	 * SC_HANDLE WINAPI OpenSCManager( LPCTSTR lpMachineName, LPCTSTR
	 * lpDatabaseName, DWORD dwDesiredAccess );
	 */
	public Pointer OpenSCManager(String lpMachineName, WString lpDatabaseName, int dwDesiredAccess);

	/*
	 * BOOL WINAPI CloseServiceHandle( SC_HANDLE hSCObject );
	 */
	public boolean CloseServiceHandle(Pointer hSCObject);

	/*
	 * SC_HANDLE WINAPI OpenService( SC_HANDLE hSCManager, LPCTSTR
	 * lpServiceName, DWORD dwDesiredAccess );
	 */
	public Pointer OpenService(Pointer hSCManager, String lpServiceName, int dwDesiredAccess);

	/*
	 * BOOL WINAPI StartService( SC_HANDLE hService, DWORD dwNumServiceArgs,
	 * LPCTSTR* lpServiceArgVectors );
	 */
	public boolean StartService(Pointer hService, int dwNumServiceArgs, char[] lpServiceArgVectors);

	/*
	 * BOOL WINAPI ControlService( SC_HANDLE hService, DWORD dwControl,
	 * LPSERVICE_STATUS lpServiceStatus );
	 */
	public boolean ControlService(Pointer hService, int dwControl, SERVICE_STATUS lpServiceStatus);

	/*
	 * BOOL WINAPI StartServiceCtrlDispatcher( const SERVICE_TABLE_ENTRY*
	 * lpServiceTable );
	 */
	public boolean StartServiceCtrlDispatcher(Structure[] lpServiceTable);

	/*
	 * SERVICE_STATUS_HANDLE WINAPI RegisterServiceCtrlHandler( LPCTSTR
	 * lpServiceName, LPHANDLER_FUNCTION lpHandlerProc );
	 */
	public Pointer RegisterServiceCtrlHandler(String lpServiceName, Handler lpHandlerProc);

	/*
	 * SERVICE_STATUS_HANDLE WINAPI RegisterServiceCtrlHandlerEx( LPCTSTR
	 * lpServiceName, LPHANDLER_FUNCTION_EX lpHandlerProc, LPVOID lpContext );
	 */
	public Pointer RegisterServiceCtrlHandlerEx(String lpServiceName, HandlerEx lpHandlerProc, Pointer lpContext);

	/*
	 * BOOL WINAPI SetServiceStatus( SERVICE_STATUS_HANDLE hServiceStatus,
	 * LPSERVICE_STATUS lpServiceStatus );
	 */
	public boolean SetServiceStatus(Pointer hServiceStatus, SERVICE_STATUS lpServiceStatus);

	/*
	 * SC_HANDLE WINAPI CreateService( SC_HANDLE hSCManager, LPCTSTR
	 * lpServiceName, LPCTSTR lpDisplayName, DWORD dwDesiredAccess, DWORD
	 * dwServiceType, DWORD dwStartType, DWORD dwErrorControl, LPCTSTR
	 * lpBinaryPathName, LPCTSTR lpLoadOrderGroup, LPDWORD lpdwTagId, LPCTSTR
	 * lpDependencies, LPCTSTR lpServiceStartName, LPCTSTR lpPassword );
	 */
	public Pointer CreateService(Pointer hSCManager, String lpServiceName, String lpDisplayName, int dwDesiredAccess, int dwServiceType,
			int dwStartType, int dwErrorControl, String lpBinaryPathName, String lpLoadOrderGroup, IntByReference lpdwTagId, String lpDependencies,
			String lpServiceStartName, String lpPassword);

	/*
	 * BOOL WINAPI DeleteService( SC_HANDLE hService );
	 */
	public boolean DeleteService(Pointer hService);

	/*
	 * BOOL WINAPI ChangeServiceConfig2( SC_HANDLE hService, DWORD dwInfoLevel,
	 * LPVOID lpInfo );
	 */
	public boolean ChangeServiceConfig2(Pointer hService, int dwInfoLevel, ChangeServiceConfig2Info lpInfo);

	/*
	 * LONG WINAPI RegOpenKeyEx( HKEY hKey, LPCTSTR lpSubKey, DWORD ulOptions,
	 * REGSAM samDesired, PHKEY phkResult );
	 */
	public int RegOpenKeyEx(int hKey, String lpSubKey, int ulOptions, int samDesired, IntByReference phkResult);

	/*
	 * LONG WINAPI RegQueryValueEx( HKEY hKey, LPCTSTR lpValueName, LPDWORD
	 * lpReserved, LPDWORD lpType, LPBYTE lpData, LPDWORD lpcbData );
	 */
	public int RegQueryValueEx(int hKey, String lpValueName, IntByReference lpReserved, IntByReference lpType, byte[] lpData, IntByReference lpcbData);

	/*
	 * LONG WINAPI RegCloseKey( HKEY hKey );
	 */
	public int RegCloseKey(int hKey);

	/*
	 * LONG WINAPI RegDeleteValue( HKEY hKey, LPCTSTR lpValueName );
	 */
	public int RegDeleteValue(int hKey, String lpValueName);

	/*
	 * LONG WINAPI RegSetValueEx( HKEY hKey, LPCTSTR lpValueName, DWORD
	 * Reserved, DWORD dwType, const BYTE* lpData, DWORD cbData );
	 */
	public int RegSetValueEx(int hKey, String lpValueName, int Reserved, int dwType, byte[] lpData, int cbData);

	/*
	 * LONG WINAPI RegCreateKeyEx( HKEY hKey, LPCTSTR lpSubKey, DWORD Reserved,
	 * LPTSTR lpClass, DWORD dwOptions, REGSAM samDesired, LPSECURITY_ATTRIBUTES
	 * lpSecurityAttributes, PHKEY phkResult, LPDWORD lpdwDisposition );
	 */
	public int RegCreateKeyEx(int hKey, String lpSubKey, int Reserved, String lpClass, int dwOptions, int samDesired,
			WINBASE.SECURITY_ATTRIBUTES lpSecurityAttributes, IntByReference phkResult, IntByReference lpdwDisposition);

	/*
	 * LONG WINAPI RegDeleteKey( HKEY hKey, LPCTSTR lpSubKey );
	 */
	public int RegDeleteKey(int hKey, String name);

	/*
	 * LONG WINAPI RegEnumKeyEx( HKEY hKey, DWORD dwIndex, LPTSTR lpName,
	 * LPDWORD lpcName, LPDWORD lpReserved, LPTSTR lpClass, LPDWORD lpcClass,
	 * PFILETIME lpftLastWriteTime );
	 */
	public int RegEnumKeyEx(int hKey, int dwIndex, char[] lpName, IntByReference lpcName, IntByReference reserved, char[] lpClass,
			IntByReference lpcClass, WINBASE.FILETIME lpftLastWriteTime);

	/*
	 * LONG WINAPI RegEnumValue( HKEY hKey, DWORD dwIndex, LPTSTR lpValueName,
	 * LPDWORD lpcchValueName, LPDWORD lpReserved, LPDWORD lpType, LPBYTE
	 * lpData, LPDWORD lpcbData );
	 */
	public int RegEnumValue(int hKey, int dwIndex, char[] lpValueName, IntByReference lpcchValueName, IntByReference reserved, IntByReference lpType,
			byte[] lpData, IntByReference lpcbData);

	interface SERVICE_MAIN_FUNCTION extends StdCallCallback
	{
		/*
		 * VOID WINAPI ServiceMain( DWORD dwArgc, LPTSTR* lpszArgv );
		 */
		public void callback(int dwArgc, Pointer lpszArgv);
	}

	interface Handler extends StdCallCallback
	{
		/*
		 * VOID WINAPI Handler( DWORD fdwControl );
		 */
		public void callback(int fdwControl);
	}

	interface HandlerEx extends StdCallCallback
	{
		/*
		 * DWORD WINAPI HandlerEx( DWORD dwControl, DWORD dwEventType, LPVOID
		 * lpEventData, LPVOID lpContext );
		 */
		public int callback(int dwControl, int dwEventType, Pointer lpEventData, Pointer lpContext);
	}

	/*
	 * typedef struct _SERVICE_STATUS { DWORD dwServiceType; DWORD
	 * dwCurrentState; DWORD dwControlsAccepted; DWORD dwWin32ExitCode; DWORD
	 * dwServiceSpecificExitCode; DWORD dwCheckPoint; DWORD dwWaitHint; }
	 * SERVICE_STATUS,LPSERVICE_STATUS;
	 */
	public static class SERVICE_STATUS extends Structure
	{
		public int	dwServiceType;
		public int	dwCurrentState;
		public int	dwControlsAccepted;
		public int	dwWin32ExitCode;
		public int	dwServiceSpecificExitCode;
		public int	dwCheckPoint;
		public int	dwWaitHint;
	}

	/*
	 * typedef struct _SERVICE_TABLE_ENTRY { LPTSTR lpServiceName;
	 * LPSERVICE_MAIN_FUNCTION lpServiceProc; } SERVICE_TABLE_ENTRY,
	 * LPSERVICE_TABLE_ENTRY;
	 */
	public static class SERVICE_TABLE_ENTRY extends Structure
	{
		public String					lpServiceName;
		public SERVICE_MAIN_FUNCTION	lpServiceProc;
	}

	public static class ChangeServiceConfig2Info extends Structure
	{
	}

	/*
	 * YAJSW additions start here
	 */

	/*
	 * BOOL WINAPI QueryServiceConfig( __in SC_HANDLE hService, __out_opt
	 * LPQUERY_SERVICE_CONFIG lpServiceConfig, __in DWORD cbBufSize, __out
	 * LPDWORD pcbBytesNeeded );
	 */
	boolean QueryServiceConfig(Pointer hService, Memory lpServiceConfig, int cbBufSize, IntByReference pcbBytesNeeded);

	/*
	 * BOOL WINAPI QueryServiceStatusEx( __in SC_HANDLE hService, __in
	 * SC_STATUS_TYPE InfoLevel, __out_opt LPBYTE lpBuffer, __in DWORD
	 * cbBufSize, __out LPDWORD pcbBytesNeeded );
	 */
	boolean QueryServiceStatusEx(Pointer hService, short InfoLevel, Memory lpBuffer, int cbBufSize, IntByReference pcbBytesNeeded);

	public static final int	SC_STATUS_PROCESS_INFO	= 0;

	/*
	 * BOOL WINAPI QueryServiceConfig2( __in SC_HANDLE hService, __in DWORD
	 * dwInfoLevel, __out_opt LPBYTE lpBuffer, __in DWORD cbBufSize, __out
	 * LPDWORD pcbBytesNeeded );
	 */
	boolean QueryServiceConfig2(Pointer hService, short InfoLevel, Memory lpBuffer, int cbBufSize, IntByReference pcbBytesNeeded);

	public static final int	SERVICE_CONFIG_DESCRIPTION	= 1;

	/*
	 * typedef struct _ENUM_SERVICE_STATUS_PROCESS { LPTSTR lpServiceName;
	 * LPTSTR lpDisplayName; SERVICE_STATUS_PROCESS ServiceStatusProcess; }
	 * ENUM_SERVICE_STATUS_PROCESS,LPENUM_SERVICE_STATUS_PROCESS;
	 */

	public static class ENUM_SERVICE_STATUS_PROCESS extends Structure
	{
		public Pointer					lpServiceName;
		public Pointer					lpDisplayName;
		public SERVICE_STATUS_PROCESS	ServiceStatusProcess;

		public void init(Pointer pointer)
		{
			useMemory(pointer);
			read();
		}

		public ENUM_SERVICE_STATUS_PROCESS next()
		{
			ENUM_SERVICE_STATUS_PROCESS next = new ENUM_SERVICE_STATUS_PROCESS();
			next.useMemory(getPointer(), size());
			next.read();
			return next;
		}

		public String getServiceName()
		{
			return lpServiceName.getString(0, true);
		}

		public String getDisplayName()
		{
			return lpDisplayName.getString(0, true);
		}

		public int getProcessId()
		{
			return ServiceStatusProcess.dwProcessId;
		}

		public int getCurrentState()
		{
			return ServiceStatusProcess.dwCurrentState;
		}

	}

	/*
	 * typedef struct _SERVICE_STATUS_PROCESS { DWORD dwServiceType; DWORD
	 * dwCurrentState; DWORD dwControlsAccepted; DWORD dwWin32ExitCode; DWORD
	 * dwServiceSpecificExitCode; DWORD dwCheckPoint; DWORD dwWaitHint; DWORD
	 * dwProcessId; DWORD dwServiceFlags; } SERVICE_STATUS_PROCESS,
	 * LPSERVICE_STATUS_PROCESS;
	 */

	public static class SERVICE_STATUS_PROCESS extends Structure
	{
		public void init(Pointer pointer)
		{
			useMemory(pointer);
			read();
		}

		public int	dwServiceType;
		public int	dwCurrentState;
		public int	dwControlsAccepted;
		public int	dwWin32ExitCode;
		public int	dwServiceSpecificExitCode;
		public int	dwCheckPoint;
		public int	dwWaitHint;
		public int	dwProcessId;
		public int	dwServiceFlags;

	}

	public static final int	SERVICE_RUNNING			= 0x00000004;
	public static final int	SERVICE_STOPPED			= 0x00000001;
	public static final int	SERVICE_PAUSED			= 0x00000007;
	public static final int	SERVICE_START_PENDING	= 0x00000002;
	public static final int	SERVICE_STOP_PENDING	= 0x00000003;

	/*
	 * typedef struct _QUERY_SERVICE_CONFIG { DWORD dwServiceType; DWORD
	 * dwStartType; DWORD dwErrorControl; LPTSTR lpBinaryPathName; LPTSTR
	 * lpLoadOrderGroup; DWORD dwTagId; LPTSTR lpDependencies; LPTSTR
	 * lpServiceStartName; LPTSTR lpDisplayName; } QUERY_SERVICE_CONFIG,
	 * LPQUERY_SERVICE_CONFIG;
	 */

	public static class QUERY_SERVICE_CONFIG extends Structure
	{
		public void init(Pointer pointer)
		{
			useMemory(pointer);
			read();
		}

		public int		dwServiceType;
		public int		dwStartType;
		public int		dwErrorControl;
		public String	lpBinaryPathName;
		public String	lpLoadOrderGroup;
		public int		dwTagId;
		public Pointer	lpDependencies;
		public String	lpServiceStartName;
		public String	lpDisplayName;

		public String[] getDependencies()
		{
			List<String> result = new ArrayList<String>();
			Pointer ptr = lpDependencies;
			int offset = 0;
			String s = "";
			if (ptr != null)
				do
				{
					s = ptr.getString(offset, true);
					if (s != null && !"".equals(s))
					{
						result.add(s);
						offset += s.getBytes().length * 2 + 2;
					}
				}
				while (s != null && !"".equals(s));
			return result.toArray(new String[0]);
		}

	}

	/*
	 * typedef struct _SERVICE_DESCRIPTION { LPTSTR lpDescription; }
	 * SERVICE_DESCRIPTION,LPSERVICE_DESCRIPTION;
	 */
	public static class SERVICE_DESCRIPTION extends ChangeServiceConfig2Info
	{
		public String	lpDescription;

		public void init(Pointer pointer)
		{
			useMemory(pointer);
			read();
		}
	}
	
	/*
	 * typedef struct _SERVICE_DELAYED_AUTO_START_INFO {
  BOOL fDelayedAutostart;}
	 */
	public static class SERVICE_DELAYED_AUTO_START_INFO extends ChangeServiceConfig2Info
	{
		public boolean	fDelayedAutostart;

		public void init(Pointer pointer)
		{
			useMemory(pointer);
			read();
		}
	}
	
	/*
	 * typedef struct _SC_ACTION {
  SC_ACTION_TYPE Type;
  DWORD          Delay;
} SC_ACTION, *LPSC_ACTION;
	 */
	public static class SC_ACTION extends Structure
	{
		  public SC_ACTION()
		  {
			  super();
			  setAutoWrite(true);
			  setAutoRead(false);
		  }
		  public int Type;
		  public int Delay;
		}
	
	// SC_ACTION types
	public static final int SC_ACTION_NONE = 0; //	No action.
	public static final int SC_ACTION_REBOOT = 2; // Reboot the computer.
	public static final int SC_ACTION_RESTART = 1; // Restart the service.
	public static final int SC_ACTION_RUN_COMMAND = 3; // Run a command.
	
	/*
	 * typedef struct _SERVICE_FAILURE_ACTIONS {
  DWORD     dwResetPeriod;
  LPTSTR    lpRebootMsg;
  LPTSTR    lpCommand;
  DWORD     cActions;
  SC_ACTION *lpsaActions;
} SERVICE_FAILURE_ACTIONS, *LPSERVICE_FAILURE_ACTIONS;
	 */
	public static class SERVICE_FAILURE_ACTIONS extends ChangeServiceConfig2Info
	{
		  public int     dwResetPeriod;
		  public String    lpRebootMsg;
		  public String    lpCommand;
		  public int     cActions;
		  public Pointer lpsaActions;

	}
	

	/*
	 * BOOL WINAPI EnumServicesStatusEx( __in SC_HANDLE hSCManager, __in
	 * SC_ENUM_TYPE InfoLevel, __in DWORD dwServiceType, __in DWORD
	 * dwServiceState, __out_opt LPBYTE lpServices, __in DWORD cbBufSize, __out
	 * LPDWORD pcbBytesNeeded, __out LPDWORD lpServicesReturned, __inout_opt
	 * LPDWORD lpResumeHandle, __in_opt LPCTSTR pszGroupName );
	 */
	public boolean EnumServicesStatusExW(Pointer hSCManager, int InfoLevel, int dwServiceType, int dwServiceState, Memory lpServices, int cbBufSize,
			IntByReference pcbBytesNeeded, IntByReference lpServicesReturned, IntByReference lpResumeHandle, String pszGroupName);

	public static final int	SERVICE_DISABLED			= 0x00000004;
	public static final int	SERVICE_INTERACTIVE_PROCESS	= 0x00000100;
	public static final int	SERVICE_AUTO_START			= 0x00000002;
	public static final int	SERVICE_BOOT_START			= 0x00000000;
	public static final int	SERVICE_DEMAND_START		= 0x00000003;
	public static final int	SERVICE_SYSTEM_START		= 0x00000001;

}
