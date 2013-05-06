/* This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * <p/>
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.  
 */
package org.rzo.yajsw.os.ms.win.w32;


import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileDescriptor;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import jnacontrib.jna.Advapi32;
import jnacontrib.jna.Options;

import org.apache.commons.collections.MultiHashMap;
import org.rzo.yajsw.io.CyclicBufferFileInputStream;
import org.rzo.yajsw.io.CyclicBufferFilePrintStream;
import org.rzo.yajsw.os.AbstractProcess;
import org.rzo.yajsw.os.Process;
import org.rzo.yajsw.os.ms.win.w32.WindowsXPProcess.MyAdvapi.TOKEN_PRIVILEGES;
import org.rzo.yajsw.os.ms.win.w32.WindowsXPProcess.MyKernel32.MEMORY_BASIC_INFORMATION;
import org.rzo.yajsw.os.ms.win.w32.WindowsXPProcess.MyKernel32.PROCESSENTRY32;
import org.rzo.yajsw.os.ms.win.w32.WindowsXPProcess.MyKernel32.PROCESS_INFORMATION;
import org.rzo.yajsw.os.ms.win.w32.WindowsXPProcess.MyKernel32.SECURITY_ATTRIBUTES;
import org.rzo.yajsw.os.ms.win.w32.WindowsXPProcess.MyKernel32.STARTUPINFO;
import org.rzo.yajsw.os.ms.win.w32.WindowsXPProcess.Ntdll.PEB;
import org.rzo.yajsw.os.ms.win.w32.WindowsXPProcess.Ntdll.PEB64;
import org.rzo.yajsw.os.ms.win.w32.WindowsXPProcess.Ntdll.PROCESS_BASIC_INFORMATION;
import org.rzo.yajsw.os.ms.win.w32.WindowsXPProcess.Ntdll.RTL_USER_PROCESS_PARAMETERS;
import org.rzo.yajsw.os.ms.win.w32.WindowsXPProcess.Shell32.SHELLEXECUTEINFO;

import com.sun.jna.Memory;
import com.sun.jna.Native;
import com.sun.jna.NativeLong;
import com.sun.jna.PlatformEx;
import com.sun.jna.Pointer;
import com.sun.jna.StringBlock;
import com.sun.jna.Structure;
import com.sun.jna.WString;
import com.sun.jna.platform.win32.Kernel32Util;
import com.sun.jna.platform.win32.User32;
import com.sun.jna.platform.win32.WinDef.HWND;
import com.sun.jna.platform.win32.WinNT.HANDLE;
import com.sun.jna.platform.win32.WinUser.WINDOWINFO;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.ptr.LongByReference;
import com.sun.jna.ptr.NativeLongByReference;
import com.sun.jna.ptr.PointerByReference;
import com.sun.jna.win32.StdCallLibrary;

// TODO: Auto-generated Javadoc
/**
 * The Class WindowsXPProcess.
 */
public class WindowsXPProcess extends AbstractProcess
{
	public interface Shell32 extends com.sun.jna.win32.StdCallLibrary
	{
		// Method declarations, constant and structure definitions go here

		/** The INSTANCE. */
		Shell32	INSTANCE	= (Shell32) Native.loadLibrary("Shell32", Shell32.class);
		
		/*
		 * BOOL ShellExecuteEx(
  __inout  LPSHELLEXECUTEINFO lpExecInfo
);
		 */
		boolean ShellExecuteEx(
				SHELLEXECUTEINFO lpExecInfo
				);
		
		/*
		 * typedef struct _SHELLEXECUTEINFO {
  DWORD     cbSize;
  ULONG     fMask;
  HWND      hwnd;
  LPCTSTR   lpVerb;
  LPCTSTR   lpFile;
  LPCTSTR   lpParameters;
  LPCTSTR   lpDirectory;
  int       nShow;
  HINSTANCE hInstApp;
  LPVOID    lpIDList;
  LPCTSTR   lpClass;
  HKEY      hkeyClass;
  DWORD     dwHotKey;
  union {
    HANDLE hIcon;
    HANDLE hMonitor;
  } DUMMYUNIONNAME;
  HANDLE    hProcess;
} SHELLEXECUTEINFO, *LPSHELLEXECUTEINFO;
		 */
		public static class SHELLEXECUTEINFO extends Structure
		{
			  public int     cbSize;
			  public int     fMask;
			  public Pointer      hwnd;
			  public String   lpVerb;
			  public String   lpFile;
			  public String   lpParameters;
			  public String   lpDirectory;
			  public int       nShow;
			  public IntByReference hInstApp;
			  public Pointer    lpIDList;
			  public String   lpClass;
			  public Pointer      hkeyClass;
			  public int     dwHotKey;
			  public Pointer hMonitor;
			  public HANDLE    hProcess;			
		}
		
		public static final int SEE_MASK_DEFAULT = 0x00000000;
		public static final int SEE_MASK_CLASSNAME = 0x00000001;
		public static final int SEE_MASK_CLASSKEY = 0x00000003;
		public static final int SEE_MASK_IDLIST = 0x00000004;
		public static final int SEE_MASK_INVOKEIDLIST = 0x0000000C;
		public static final int SEE_MASK_ICON = 0x00000010;
		public static final int SEE_MASK_HOTKEY = 0x00000020;
		public static final int SEE_MASK_NOCLOSEPROCESS = 0x00000040;
		public static final int SEE_MASK_CONNECTNETDRV = 0x00000080;
		public static final int SEE_MASK_NOASYNC = 0x00000100;
		public static final int SEE_MASK_FLAG_DDEWAIT = 0x00000100;
		public static final int SEE_MASK_DOENVSUBST = 0x00000200;
		public static final int SEE_MASK_FLAG_NO_UI = 0x00000400;
		public static final int SEE_MASK_UNICODE = 0x00004000;
		public static final int SEE_MASK_NO_CONSOLE = 0x00008000;
		public static final int SEE_MASK_ASYNCOK = 0x00100000;
		public static final int SEE_MASK_NOQUERYCLASSSTORE = 0x01000000;
		public static final int SEE_MASK_HMONITOR = 0x00200000;
		public static final int SEE_MASK_NOZONECHECKS = 0x00800000;
		public static final int SEE_MASK_WAITFORINPUTIDLE = 0x02000000;
		public static final int SEE_MASK_FLAG_LOG_USAGE = 0x04000000;

		public static final String VERB_EDIT = "edit";
		public static final String VERB_EXPLORE = "explore";
		public static final String VERB_FIND = "find";
		public static final String VERB_OPEN = "open";
		public static final String VERB_PRINT = "print";
		public static final String VERB_PROPERTIES = "properties";
		public static final String VERB_RUNAS = "runas";
		
		public static final int SW_HIDE = 0;
		public static final int SW_MAXIMIZE = 3;
		public static final int SW_MINIMIZE = 6;
		public static final int SW_RESTORE = 9;
		public static final int SW_SHOW = 5;
		public static final int SW_SHOWDEFAULT = 10;
		public static final int SW_SHOWMAXIMIZED = 3;
		public static final int SW_SHOWMINIMIZED = 2;
		public static final int SW_SHOWMINNOACTIVE = 7;
		public static final int SW_SHOWNA = 8;
		public static final int SW_SHOWNOACTIVATE = 4;
		public static final int SW_SHOWNORMAL = 1;

		
	}



	/**
	 * The Interface MyUser32.
	 */
	public interface MyUser32 extends User32
	{
		// Method declarations, constant and structure definitions go here

		/** The INSTANCE. */
		MyUser32	INSTANCE	= (MyUser32) Native.loadLibrary("User32", MyUser32.class);

		/*
		 * HWND GetForegroundWindow(VOID);
		 */
		/**
		 * Gets the foreground window.
		 * 
		 * @return the pointer
		 */
		HWND GetForegroundWindow();

		/*
		 * DWORD GetWindowThreadProcessId( HWND hWnd, LPDWORD lpdwProcessId );
		 */
		/**
		 * Gets the window thread process id.
		 * 
		 * @param hWnd
		 *            the h wnd
		 * @param lpdwProcessId
		 *            the lpdw process id
		 * 
		 * @return the int
		 */
		int GetWindowThreadProcessId(Pointer hWnd, IntByReference lpdwProcessId);

		/** The W m_ close. */
		int	WM_CLOSE	= 16;
		int	WM_QUIT		= 18;
		int	WM_DESTROY	= 2;
		int WM_KEYDOWN = 256;
		int WM_KEYUP = 257;
		int WM_CHAR = 258;
		int WM_SETFOCUS = 7;
		
		int VK_CONTROL	= 17;
		
		int GetWindowTextA(HWND hWnd, byte[] lpString, int nMaxCount);


		/*
		 * BOOL WINAPI GetWindowInfo(
  __in     HWND hwnd,
  __inout  PWINDOWINFO pwi
);
		 */
		boolean GetWindowInfo(
				  HWND hwnd,
				  WINDOWINFO pwi
				);
		
		/*
		 * BOOL PostThreadMessage( DWORD idThread, UINT Msg, WPARAM wParam,
		 * LPARAM lParam );
		 */
		/**
		 * Post thread message a.
		 * 
		 * @param idThread
		 *            the id thread
		 * @param Msg
		 *            the msg
		 * @param wParam
		 *            the w param
		 * @param lParam
		 *            the l param
		 * 
		 * @return true, if successful
		 */
		boolean PostThreadMessageA(int idThread, int Msg, int wParam, int lParam);

		/*
		 * DWORD WINAPI WaitForInputIdle( __in HANDLE hProcess, __in DWORD
		 * dwMilliseconds );
		 */
		/**
		 * Wait for input idle.
		 * 
		 * @param hProcess
		 *            the h process
		 * @param dwMilliseconds
		 *            the dw milliseconds
		 * 
		 * @return the int
		 */
		int WaitForInputIdle(HANDLE hProcess, int dwMilliseconds);

		void PostMessageA(HWND hWnd, int msg, Pointer wParam, Pointer lParam);
		void SendMessageW(HWND hWnd, int msg, long wParam, int lParam);
		public interface WNDENUMPROC extends StdCallCallback
		{
			/** Return whether to continue enumeration. */
			boolean callback(HWND hWnd, int data);
		}

		boolean EnumWindows(WNDENUMPROC lpEnumFunc, int data);

	}

	/**
	 * The Interface MyKernel32.
	 */
	public interface MyKernel32 extends com.sun.jna.platform.win32.Kernel32
	{

		// Method declarations, constant and structure definitions go here

		/** The INSTANCE. */
		MyKernel32	INSTANCE	= (MyKernel32) Native.loadLibrary("kernel32", MyKernel32.class);

		/*
		 * BOOL WINAPI ReadFile( __in HANDLE hFile, __out LPVOID lpBuffer, __in
		 * DWORD nNumberOfBytesToRead, __out_opt LPDWORD lpNumberOfBytesRead,
		 * __inout_opt LPOVERLAPPED lpOverlapped );
		 */

		boolean ReadFile(Pointer hFile, Memory lpBuffer, int nNumberOfBytesToRead, IntByReference lpNumberOfBytesRead, Structure lpOverlapped);

		/*
		 * DWORD WINAPI GetCurrentProcessId(void);
		 */
		/*
		 * (non-Javadoc)
		 * 
		 * @see com.sun.jna.examples.win32.Kernel32#GetCurrentProcessId()
		 */
		int GetCurrentProcessId();

		/*
		 * DWORD WINAPI GetProcessIdOfThread( __in HANDLE Thread );
		 */
		/**
		 * Gets the process id of thread.
		 * 
		 * @param Thread
		 *            the thread
		 * 
		 * @return the int
		 */
		int GetProcessIdOfThread(Pointer Thread);

		/*
		 * BOOL WINAPI CreateProcess( LPCTSTR lpApplicationName, LPTSTR
		 * lpCommandLine, LPSECURITY_ATTRIBUTES lpProcessAttributes,
		 * LPSECURITY_ATTRIBUTES lpThreadAttributes, BOOL bInheritHandles, DWORD
		 * dwCreationFlags, LPVOID lpEnvironment, LPCTSTR lpCurrentDirectory,
		 * LPSTARTUPINFO lpStartupInfo, LPPROCESS_INFORMATION
		 * lpProcessInformation );
		 */
		/**
		 * Creates the process a.
		 * 
		 * @param lpApplicationName
		 *            the lp application name
		 * @param lpCommandLine
		 *            the lp command line
		 * @param lpProcessAttributes
		 *            the lp process attributes
		 * @param lpThreadAttributes
		 *            the lp thread attributes
		 * @param bInheritHandles
		 *            the b inherit handles
		 * @param dwCreationFlags
		 *            the dw creation flags
		 * @param lpEnvironment
		 *            the lp environment
		 * @param lpCurrentDirectory
		 *            the lp current directory
		 * @param lpStartupInfo
		 *            the lp startup info
		 * @param lpProcessInformation
		 *            the lp process information
		 * 
		 * @return true, if successful
		 */
		boolean CreateProcessA(String lpApplicationName, String lpCommandLine, Structure lpProcessAttributes, Structure lpThreadAttributes,
				boolean bInheritHandles, int dwCreationFlags, Structure lpEnvironment, String lpCurrentDirectory, Structure lpStartupInfo,
				Structure lpProcessInformation);

		boolean CreateProcessW(WString lpApplicationName, WString lpCommandLine, Structure lpProcessAttributes, Structure lpThreadAttributes,
				boolean bInheritHandles, int dwCreationFlags, Memory lpEnvironment, WString lpCurrentDirectory, Structure lpStartupInfo,
				Structure lpProcessInformation);

		/** The CREAT e_ n o_ window. */
		int	CREATE_NO_WINDOW			= 0x08000000;

		/** The CREAT e_ unicod e_ environment. */
		int	CREATE_UNICODE_ENVIRONMENT	= 0x00000400;

		/** The CREAT e_ ne w_ console. */
		int	CREATE_NEW_CONSOLE			= 0x00000010;

		int	DETACHED_PROCESS			= 0x00000008;

		/*
		 * typedef struct _PROCESS_INFORMATION { HANDLE hProcess; HANDLE
		 * hThread; DWORD dwProcessId; DWORD dwThreadId; }
		 */

		/**
		 * The Class PROCESS_INFORMATION.
		 */
		public static class PROCESS_INFORMATION extends Structure
		{

			/** The h process. */
			public HANDLE	hProcess	= null;

			/** The h thread. */
			public Pointer	hThread		= null;

			/** The dw process id. */
			public int		dwProcessId	= -1;

			/** The dw thread id. */
			public int		dwThreadId	= -1;
			
			@Override
			public void finalize()
			{
				try
				{
					super.finalize();
				}
				catch (Throwable e)
				{
					e.printStackTrace();
				}
			}

		}

		/*
		 * typedef struct _STARTUPINFO { DWORD cb; LPTSTR lpReserved; LPTSTR
		 * lpDesktop; LPTSTR lpTitle; DWORD dwX; DWORD dwY; DWORD dwXSize; DWORD
		 * dwYSize; DWORD dwXCountChars; DWORD dwYCountChars; DWORD
		 * dwFillAttribute; DWORD dwFlags; WORD wShowWindow; WORD cbReserved2;
		 * LPBYTE lpReserved2; HANDLE hStdInput; HANDLE hStdOutput; HANDLE
		 * hStdError; }
		 */

		/**
		 * The Class STARTUPINFO.
		 */
		public static class STARTUPINFO extends Structure
		{

			/** The cb. */
			public int		cb;

			/** The lp reserved. */
			public WString	lpReserved;

			/** The lp desktop. */
			public WString	lpDesktop;

			/** The lp title. */
			public WString	lpTitle;

			/** The dw x. */
			public int		dwX;

			/** The dw y. */
			public int		dwY;

			/** The dw x size. */
			public int		dwXSize;

			/** The dw y size. */
			public int		dwYSize;

			/** The dw x count chars. */
			public int		dwXCountChars;

			/** The dw y count chars. */
			public int		dwYCountChars;

			/** The dw fill attribute. */
			public int		dwFillAttribute;

			/** The dw flags. */
			public int		dwFlags;

			/** The w show window. */
			public short	wShowWindow;

			/** The cb reserved2. */
			public short	cbReserved2;

			/** The lp reserved2. */
			public Pointer	lpReserved2;

			/** The h std input. */
			public Pointer	hStdInput;

			/** The h std output. */
			public Pointer	hStdOutput;

			/** The h std error. */
			public Pointer	hStdError;
			
			@Override
			public void finalize()
			{
				try
				{
					super.finalize();
				}
				catch (Throwable e)
				{
					e.printStackTrace();
				}
			}
			
			@Override
			public String toString()
			{
				return ""+hStdError+"/"+hStdOutput+"/"+hStdInput;
			}

		}

		int	SW_SHOWMINIMIZED			= 2;
		int	SW_SHOWNOACTIVATE			= 4;

		int	STARTF_USESHOWWINDOW		= 0x00000001;

		/** The START f_ usestdhandles. */
		int	STARTF_USESTDHANDLES		= 256;

		/** The IDL e_ priorit y_ class. */
		int	IDLE_PRIORITY_CLASS			= 0x00000040;

		/** The BELO w_ norma l_ priorit y_ class. */
		int	BELOW_NORMAL_PRIORITY_CLASS	= 0x00004000;

		/** The NORMA l_ priorit y_ class. */
		int	NORMAL_PRIORITY_CLASS		= 0x00000020;

		/** The ABOV e_ norma l_ priorit y_ class. */
		int	ABOVE_NORMAL_PRIORITY_CLASS	= 0x00008000;

		/** The HIG h_ priorit y_ class. */
		int	HIGH_PRIORITY_CLASS			= 0x00000080;

		/** The REALTIM e_ priorit y_ class. */
		int	REALTIME_PRIORITY_CLASS		= 0x00000100;

		/*
		 * DWORD WINAPI WaitForSingleObject( HANDLE hHandle, DWORD
		 * dwMilliseconds );
		 */
		/**
		 * Wait for single object.
		 * 
		 * @param handle
		 *            the handle
		 * @param dwMilliseconds
		 *            the dw milliseconds
		 * 
		 * @return the int
		 */
		int WaitForSingleObject(Pointer handle, int dwMilliseconds);

		/** The INFINITE. */
		int	INFINITE	= 0xFFFFFFFF;

		/*
		 * BOOL WINAPI GetExitCodeProcess( HANDLE hProcess, LPDWORD lpExitCode
		 * );
		 */
		/**
		 * Gets the exit code process.
		 * 
		 * @param handle
		 *            the h process
		 * @param lpExitCode
		 *            the lp exit code
		 * 
		 * @return true, if successful
		 */
		boolean GetExitCodeProcess(HANDLE handle, IntByReference lpExitCode);

		/** The STIL l_ active. */
		int	STILL_ACTIVE	= 0x103;

		/*
		 * BOOL WINAPI TerminateProcess( HANDLE hProcess, UINT uExitCode );
		 */

		/**
		 * Terminate process.
		 * 
		 * @param handle
		 *            the h process
		 * @param uExitCode
		 *            the u exit code
		 * 
		 * @return true, if successful
		 */
		boolean TerminateProcess(HANDLE handle, int uExitCode);

		/*
		 * BOOL WINAPI GetProcessAffinityMask( __in HANDLE hProcess, __out
		 * PDWORD_PTR lpProcessAffinityMask, __out PDWORD_PTR
		 * lpSystemAffinityMask );
		 */
		/**
		 * Gets the process affinity mask.
		 * 
		 * @param handle
		 *            the h process
		 * @param lpProcessAffinityMask
		 *            the lp process affinity mask
		 * @param lpSystemAffinityMask
		 *            the lp system affinity mask
		 * 
		 * @return true, if successful
		 */
		boolean GetProcessAffinityMask(HANDLE handle, IntByReference lpProcessAffinityMask, IntByReference lpSystemAffinityMask);

		/*
		 * BOOL WINAPI SetProcessAffinityMask( __in HANDLE hProcess, __in
		 * DWORD_PTR dwProcessAffinityMask );
		 */
		/**
		 * Sets the process affinity mask.
		 * 
		 * @param handle
		 *            the h process
		 * @param dwProcessAffinityMask
		 *            the dw process affinity mask
		 * 
		 * @return true, if successful
		 */
		boolean SetProcessAffinityMask(HANDLE handle, int dwProcessAffinityMask);

		/*
		 * BOOL WINAPI CloseHandle( HANDLE hObject );
		 */
		/**
		 * Close handle.
		 * 
		 * @param hObject
		 *            the h object
		 * 
		 * @return true, if successful
		 */
		public boolean CloseHandle(Pointer hObject);

		/*
		 * HANDLE WINAPI CreateToolhelp32Snapshot( DWORD dwFlags, DWORD
		 * th32ProcessID );
		 */
		/**
		 * Creates the toolhelp32 snapshot.
		 * 
		 * @param dwFlags
		 *            the dw flags
		 * @param th32ProcessID
		 *            the th32 process id
		 * 
		 * @return the pointer
		 */
		Pointer CreateToolhelp32Snapshot(int dwFlags, int th32ProcessID);

		/** The T h32 c s_ snapprocess. */
		int	TH32CS_SNAPPROCESS	= 0x2;

		int	WAIT_FAILED			= 0xFFFFFFFF;
		int	WAIT_TIMEOUT		= 0x00000102;
		int	WAIT_OBJECT_0		= 0x00000000;
		int	WAIT_ABANDONED		= 0x00000080;

		/*
		 * BOOL WINAPI Process32First( HANDLE hSnapshot, LPPROCESSENTRY32 lppe
		 * );
		 */
		/**
		 * Process32 first.
		 * 
		 * @param hSnapshot
		 *            the h snapshot
		 * @param lppe
		 *            the lppe
		 * 
		 * @return true, if successful
		 */
		boolean Process32First(Pointer hSnapshot, Structure lppe);

		/*
		 * typedef struct tagPROCESSENTRY32 { DWORD dwSize; DWORD cntUsage;
		 * DWORD th32ProcessID; ULONG_PTR th32DefaultHeapID; DWORD th32ModuleID;
		 * DWORD cntThreads; DWORD th32ParentProcessID; LONG pcPriClassBase;
		 * DWORD dwFlags; TCHAR szExeFile[MAX_PATH]; } PROCESSENTRY32,
		 * PPROCESSENTRY32;
		 */

		/**
		 * The Class PROCESSENTRY32.
		 */
		public static class PROCESSENTRY32 extends Structure
		{

			/** The dw size. */
			public int		dwSize;

			/** The cnt usage. */
			public int		cntUsage;

			/** The th32 process id. */
			public int		th32ProcessID;

			/** The th32 default heap id. */
			public int		th32DefaultHeapID;

			/** The th32 module id. */
			public int		th32ModuleID;

			/** The cnt threads. */
			public int		cntThreads;

			/** The th32 parent process id. */
			public int		th32ParentProcessID;

			/** The pc pri class base. */
			public int		pcPriClassBase;

			/** The dw flags. */
			public int		dwFlags;

			/** The sz exe file. */
			public char[]	szExeFile;
		}

		/** The MA x_ path. */
		int	MAX_PATH	= 260;

		/*
		 * BOOL WINAPI Process32Next( HANDLE hSnapshot, LPPROCESSENTRY32 lppe );
		 */
		/**
		 * Process32 next.
		 * 
		 * @param hSnapshot
		 *            the h snapshot
		 * @param lppe
		 *            the lppe
		 * 
		 * @return true, if successful
		 */
		boolean Process32Next(Pointer hSnapshot, Structure lppe);

		/*
		 * HANDLE WINAPI OpenProcess( DWORD dwDesiredAccess, BOOL
		 * bInheritHandle, DWORD dwProcessId );
		 */
		/**
		 * Open process.
		 * 
		 * @param dwDesiredAccess
		 *            the dw desired access
		 * @param bInheritHandle
		 *            the b inherit handle
		 * @param dwProcessId
		 *            the dw process id
		 * 
		 * @return the pointer
		 */
		HANDLE OpenProcess(int dwDesiredAccess, boolean bInheritHandle, int dwProcessId);

		/** The PROCES s_ terminate. */
		int	PROCESS_TERMINATE			= 1;

		/** The PROCES s_ quer y_ information. */
		int	PROCESS_QUERY_INFORMATION	= 1024;

		/** The STANDAR d_ right s_ required. */
		int	STANDARD_RIGHTS_REQUIRED	= 0xF0000;

		/** The SYNCHRONIZE. */
		int	SYNCHRONIZE					= 0x100000;

		/** The PROCES s_ al l_ access. */
		int	PROCESS_ALL_ACCESS			= STANDARD_RIGHTS_REQUIRED | SYNCHRONIZE | 0xFFF;

		/*
		 * BOOL WINAPI GetProcessTimes( __in HANDLE hProcess, __out LPFILETIME
		 * lpCreationTime, __out LPFILETIME lpExitTime, __out LPFILETIME
		 * lpKernelTime, __out LPFILETIME lpUserTime );
		 */
		/**
		 * Gets the process times.
		 * 
		 * @param handle
		 *            the h process
		 * @param lpCreationTime
		 *            the lp creation time
		 * @param lpExitTime
		 *            the lp exit time
		 * @param lpKernelTime
		 *            the lp kernel time
		 * @param lpUserTime
		 *            the lp user time
		 * 
		 * @return true, if successful
		 */
		boolean GetProcessTimes(HANDLE handle, LongByReference lpCreationTime, LongByReference lpExitTime, LongByReference lpKernelTime,
				LongByReference lpUserTime);

		/*
		 * BOOL WINAPI CreatePipe( __out PHANDLE hReadPipe, __out PHANDLE
		 * hWritePipe, __in LPSECURITY_ATTRIBUTES lpPipeAttributes, __in DWORD
		 * nSize );
		 */
		/**
		 * Creates the pipe.
		 * 
		 * @param hReadPipe
		 *            the h read pipe
		 * @param hWritePipe
		 *            the h write pipe
		 * @param lpPipeAttributes
		 *            the lp pipe attributes
		 * @param nSize
		 *            the n size
		 * 
		 * @return the int
		 */
		int CreatePipe(PointerByReference hReadPipe, PointerByReference hWritePipe, Structure lpPipeAttributes, int nSize);

		/*
		 * typedef struct _SECURITY_ATTRIBUTES { DWORD nLength; LPVOID
		 * lpSecurityDescriptor; BOOL bInheritHandle; } SECURITY_ATTRIBUTES,
		 * PSECURITY_ATTRIBUTES, LPSECURITY_ATTRIBUTES;
		 */

		/**
		 * The Class SECURITY_ATTRIBUTES.
		 */
		public static class SECURITY_ATTRIBUTES extends Structure
		{

			/** The n length. */
			public int		nLength;

			/** The lp security descriptor. */
			public Pointer	lpSecurityDescriptor;

			/** The b inherit handle. */
			public boolean	bInheritHandle;
		}

		/*
		 * BOOL WINAPI SetHandleInformation( __in HANDLE hObject, __in DWORD
		 * dwMask, __in DWORD dwFlags );
		 */
		/**
		 * Sets the handle information.
		 * 
		 * @param hObject
		 *            the h object
		 * @param dwMask
		 *            the dw mask
		 * @param dwFlags
		 *            the dw flags
		 * 
		 * @return true, if successful
		 */
		boolean SetHandleInformation(Pointer hObject, int dwMask, int dwFlags);

		/** The HANDL e_ fla g_ inherit. */
		int	HANDLE_FLAG_INHERIT				= 0x00000001;
		int	HANDLE_FLAG_PROTECT_FROM_CLOSE	= 0x00000002;

		/*
		 * HANDLE WINAPI CreateNamedPipe( __in LPCTSTR lpName, __in DWORD
		 * dwOpenMode, __in DWORD dwPipeMode, __in DWORD nMaxInstances, __in
		 * DWORD nOutBufferSize, __in DWORD nInBufferSize, __in DWORD
		 * nDefaultTimeOut, __in_opt LPSECURITY_ATTRIBUTES lpSecurityAttributes
		 * );
		 */
		/**
		 * Creates the named pipe a.
		 * 
		 * @param lpName
		 *            the lp name
		 * @param dwOpenMode
		 *            the dw open mode
		 * @param dwPipeMode
		 *            the dw pipe mode
		 * @param nMaxInstances
		 *            the n max instances
		 * @param nOutBufferSize
		 *            the n out buffer size
		 * @param nInBufferSize
		 *            the n in buffer size
		 * @param nDefaultTimeOut
		 *            the n default time out
		 * @param lpSecurityAttributes
		 *            the lp security attributes
		 * 
		 * @return the pointer
		 */
		Pointer CreateNamedPipeA(String lpName, int dwOpenMode, int dwPipeMode, int nMaxInstances, int nOutBufferSize, int nInBufferSize,
				int nDefaultTimeOut, SECURITY_ATTRIBUTES lpSecurityAttributes);

		/** The PIP e_ acces s_ outbound. */
		int	PIPE_ACCESS_OUTBOUND	= 0x00000002;

		/** The PIP e_ acces s_ inbound. */
		int	PIPE_ACCESS_INBOUND		= 0x00000001;

		/** The PIP e_ wait. */
		int	PIPE_WAIT				= 0x00000000;

		/** The PIP e_ nowait. */
		int	PIPE_NOWAIT				= 0x00000001;

		/** The GENERI c_ read. */
		int	GENERIC_READ			= 0x80000000;

		/**
		 * Creates the file a.
		 * 
		 * @param lpFileName
		 *            the lp file name
		 * @param dwDesiredAccess
		 *            the dw desired access
		 * @param dwShareMode
		 *            the dw share mode
		 * @param lpSecurityAttributes
		 *            the lp security attributes
		 * @param dwCreationDisposition
		 *            the dw creation disposition
		 * @param dwFlagsAndAttributes
		 *            the dw flags and attributes
		 * @param hTemplateFile
		 *            the h template file
		 * 
		 * @return the pointer
		 */
		Pointer CreateFileA(String lpFileName, int dwDesiredAccess, int dwShareMode, SECURITY_ATTRIBUTES lpSecurityAttributes,
				int dwCreationDisposition, int dwFlagsAndAttributes, Pointer hTemplateFile);

		/*
		 * BOOL WINAPI ConnectNamedPipe( __in HANDLE hNamedPipe, __inout_opt
		 * LPOVERLAPPED lpOverlapped );
		 */
		/**
		 * Connect named pipe.
		 * 
		 * @param hNamedPipe
		 *            the h named pipe
		 * @param lpOverlapped
		 *            the lp overlapped
		 * 
		 * @return true, if successful
		 */
		boolean ConnectNamedPipe(Pointer hNamedPipe, PointerByReference lpOverlapped);

		/** The INVALI d_ handl e_ value. */
		Pointer	INVALID_HANDLE_VALUE	= Pointer.createConstant(-1);

		/*
		 * BOOL WINAPI WaitNamedPipe( __in LPCTSTR lpNamedPipeName, __in DWORD
		 * nTimeOut );
		 */
		/**
		 * Wait named pipe a.
		 * 
		 * @param lpNamedPipeName
		 *            the lp named pipe name
		 * @param nTimeOut
		 *            the n time out
		 * 
		 * @return true, if successful
		 */
		boolean WaitNamedPipeA(String lpNamedPipeName, int nTimeOut);

		/** The NMPWAI t_ us e_ defaul t_ wait. */
		int	NMPWAIT_USE_DEFAULT_WAIT	= 0;

		/** The NMPWAI t_ wai t_ forever. */
		int	NMPWAIT_WAIT_FOREVER		= 0xffffffff;

		/*
		 * BOOL WINAPI SetCurrentDirectory( __in LPCTSTR lpPathName );
		 */
		boolean SetCurrentDirectoryA(String lpPathName);

		/*
		 * typedef struct _MEMORY_BASIC_INFORMATION { PVOID BaseAddress; PVOID
		 * AllocationBase; DWORD AllocationProtect; SIZE_T RegionSize; DWORD
		 * State; DWORD Protect; DWORD Type; } MEMORY_BASIC_INFORMATION,
		 * *PMEMORY_BASIC_INFORMATION;
		 */
		public static class MEMORY_BASIC_INFORMATION extends Structure
		{
			public Pointer	BaseAddress;
			public Pointer	AllocationBase;
			public int		AllocationProtect;
			public Pointer		RegionSize;
			public int		State;
			public int		Protect;
			public int		Type;
		}

		public static int	PAGE_NOACCESS	= 0x01;
		public static int	PAGE_EXECUTE	= 0x10;

		/*
		 * SIZE_T WINAPI VirtualQueryEx( __in HANDLE hProcess, __in_opt LPCVOID
		 * lpAddress, __out PMEMORY_BASIC_INFORMATION lpBuffer, __in SIZE_T
		 * dwLength );
		 */
		int VirtualQueryEx(Pointer hProcess, Pointer lpAddress, Pointer lpBuffer, int dwLength);
		
		boolean ReadProcessMemory(Pointer hProcess, Pointer lpBaseAddress, Pointer lpBuffer,  NativeLong nSize, NativeLongByReference lpNumberOfBytesRead);

		/*
		 * DWORD WTSGetActiveConsoleSessionId(void);
		 */
		int WTSGetActiveConsoleSessionId();

	}

	/**
	 * The Interface Ntdll.
	 */
	public interface Ntdll extends com.sun.jna.win32.StdCallLibrary
	{

		/** The INSTANCE. */
		Ntdll	INSTANCE	= (Ntdll) Native.loadLibrary("Ntdll", Ntdll.class);

		/*
		 * NTOSAPI NTSTATUS NTAPI ZwReadVirtualMemory( /IN/ HANDLE
		 * ProcessHandle, /IN/ PVOID BaseAddress, /OUT/ PVOID Buffer, /IN/ ULONG
		 * BufferLength, /OUT/ PULONG ReturnLength /OPTIONAL/);
		 */

		/**
		 * Zw read virtual memory.
		 * 
		 * @param ProcessHandle
		 *            the process handle
		 * @param BaseAddress
		 *            the base address
		 * @param Buffer
		 *            the buffer
		 * @param BufferLength
		 *            the buffer length
		 * @param ReturnLength
		 *            the return length
		 * 
		 * @return the int
		 */
		int ZwReadVirtualMemory(Pointer ProcessHandle, Pointer BaseAddress, Pointer Buffer, int BufferLength, IntByReference ReturnLength);

		/*
		 * NTSTATUS WINAPI ZwQueryInformationProcess( __in HANDLE ProcessHandle,
		 * __in PROCESSINFOCLASS ProcessInformationClass, __out PVOID
		 * ProcessInformation, __in ULONG ProcessInformationLength, __out_opt
		 * PULONG ReturnLength );
		 */
		/**
		 * Zw query information process.
		 * 
		 * @param process
		 *            the process handle
		 * @param ProcessInformationClass
		 *            the process information class
		 * @param ProcessInformation
		 *            the process information
		 * @param ProcessInformationLength
		 *            the process information length
		 * @param ReturnLength
		 *            the return length
		 * 
		 * @return the int
		 */
		int ZwQueryInformationProcess(HANDLE process, int ProcessInformationClass, Pointer ProcessInformation, int ProcessInformationLength,
				IntByReference ReturnLength);

		/*
		 * typedef struct _PROCESS_BASIC_INFORMATION { PVOID Reserved1; PPEB
		 * PebBaseAddress; PVOID Reserved2[2]; ULONG_PTR UniqueProcessId; PVOID
		 * Reserved3; } PROCESS_BASIC_INFORMATION;
		 */
		/**
		 * The Class PROCESS_BASIC_INFORMATION.
		 */
		class PROCESS_BASIC_INFORMATION extends Structure
		{

			/** The Reserved1. */
			public Pointer	Reserved1;

			/** The Peb base address. */
			public Pointer	PebBaseAddress;

			/** The Reserved2. */
			public Pointer[]	Reserved2	= new Pointer[2];

			/** The Unique process id. */
			public Pointer	UniqueProcessId;

			/** The Reserved3. */
			public Pointer	Reserved3;
		}

		/*
		 * typedef struct _PEB { BYTE Reserved1[2]; BYTE BeingDebugged; BYTE
		 * Reserved2[1]; PVOID Reserved3[2]; PPEB_LDR_DATA Ldr;
		 * PRTL_USER_PROCESS_PARAMETERS ProcessParameters; BYTE Reserved4[104];
		 * PVOID Reserved5[52]; PPS_POST_PROCESS_INIT_ROUTINE
		 * PostProcessInitRoutine; BYTE Reserved6[128]; PVOID Reserved7[1];
		 * ULONG SessionId; } PEB, PPEB;
		 */
		/**
		 * The Class PEB.
		 */
		class PEB extends Structure
		{

			/** The Reserved1. */
			public byte[]	Reserved1	= new byte[2];

			/** The Being debugged. */
			public byte		BeingDebugged;

			/** The Reserved2. */
			public byte		Reserved2;

			/** The Reserved3. */
			public Pointer[]	Reserved3	= new Pointer[2];

			/** The Ldr. */
			public Pointer	Ldr;

			/** The Process parameters. */
			public Pointer	ProcessParameters;

			/** The Reserved4. */
			public byte[]	Reserved4	= new byte[104];

			/** The Reserved5. */
			public Pointer[]	Reserved5	= new Pointer[52];

			/** The Post process init routine. */
			public Pointer	PostProcessInitRoutine;

			/** The Reserved6. */
			public byte[]	Reserved6	= new byte[128];

			/** The Reserved7. */
			public Pointer	Reserved7;

			/** The Session id. */
			public int		SessionId;

		}

		/*
		 * typedef struct _PEB { BYTE Reserved1[2]; BYTE BeingDebugged; BYTE
		 * Reserved2[21]; PPEB_LDR_DATA LoaderData; PRTL_USER_PROCESS_PARAMETERS
		 * ProcessParameters; BYTE Reserved3[520]; PPS_POST_PROCESS_INIT_ROUTINE
		 * PostProcessInitRoutine; BYTE Reserved4[136]; ULONG SessionId; } PEB;
		 */
		class PEB64 extends Structure
		{
			public byte[]	Reserved1	= new byte[2];
			public byte		BeingDebugged;
			public byte[]	Reserved2	= new byte[21];		;
			public Pointer	Ldr;
			public Pointer	ProcessParameters;
			public byte[]	Reserved3	= new byte[520];
			public Pointer	PostProcessInitRoutine;
			public byte[]	Reserved4	= new byte[136];
			public int		SessionId;
		}

		/*
		 * typedef struct _RTL_USER_PROCESS_PARAMETERS { BYTE Reserved1[16]; 16
		 * PVOID Reserved2[10]; 40 UNICODE_STRING ImagePathName; UNICODE_STRING
		 * CommandLine; } RTL_USER_PROCESS_PARAMETERS,
		 * PRTL_USER_PROCESS_PARAMETERS;
		 * 
		 * typedef struct _RTL_USER_PROCESS_PARAMETERS { ULONG MaximumLength; 4
		 * ULONG Length; 8 ULONG Flags; 12 ULONG DebugFlags; 16 PVOID
		 * ConsoleHandle; 4 ULONG ConsoleFlags; 8 HANDLE StdInputHandle; 12
		 * HANDLE StdOutputHandle; 16 HANDLE StdErrorHandle; 20 UNICODE_STRING
		 * CurrentDirectoryPath; 28 HANDLE CurrentDirectoryHandle; 32
		 * UNICODE_STRING DllPath; 40 UNICODE_STRING ImagePathName; 48
		 * UNICODE_STRING CommandLine; 56 PVOID Environment; 4 ULONG
		 * StartingPositionLeft; 8 ULONG StartingPositionTop; 12 ULONG Width; 16
		 * ULONG Height; 20 ULONG CharWidth; 24 ULONG CharHeight; 28 ULONG
		 * ConsoleTextAttributes; 32 ULONG WindowFlags; 36 ULONG
		 * ShowWindowFlags; 40 UNICODE_STRING WindowTitle; 48 UNICODE_STRING
		 * DesktopName; UNICODE_STRING ShellInfo; UNICODE_STRING RuntimeData;
		 * RTL_DRIVE_LETTER_CURDIR DLCurrentDirectory[0x20]; }
		 * RTL_USER_PROCESS_PARAMETERS,PRTL_USER_PROCESS_PARAMETERS;
		 */

		/**
		 * The Class RTL_USER_PROCESS_PARAMETERS.
		 */
		class RTL_USER_PROCESS_PARAMETERS extends Structure
		{
			public int AllocationSize;
			public int Size;
			public int Flags;
			public int DebugFlags;

			public HANDLE hConsole;

			public int ProcessGroup;

			public HANDLE hStdInput;
			public HANDLE hStdOutput;
			public HANDLE hStdError;

			public UNICODE_STRING CurrentDirectoryPath;
			public Pointer CurrentDirectoryHandle;
			public UNICODE_STRING DllPath;

			/** The Image path name. */
			public UNICODE_STRING ImagePathName;

			/** The Command line. */
			public UNICODE_STRING CommandLine;
			public Pointer Environment; // new

			public int dwX;
			public int dwY;
			public int dwXSize;
			public int dwYSize;
			public int dwXCountChars;
			public int dwYCountChars;
			public int dwFillAttribute;
			public int dwFlags;
			public int wShowWindow;	

			public UNICODE_STRING WindowTitle;
			public UNICODE_STRING Desktop;

			public UNICODE_STRING ShellInfo;
			public UNICODE_STRING RuntimeInfo;
			public RTL_DRIVE_LETTER_CURDIR[] DLCurrentDirectory = new RTL_DRIVE_LETTER_CURDIR[0x20];
		}
		
		class  RTL_DRIVE_LETTER_CURDIR  extends Structure 
		{
			public int Flags;
			public int Length;
			public int TimeStamp;
			public UNICODE_STRING DosPath;
	        };

		/*
		 * typedef struct _LSA_UNICODE_STRING { USHORT Length; USHORT
		 * MaximumLength; PWSTR Buffer; } LSA_UNICODE_STRING,
		 * PLSA_UNICODE_STRING, UNICODE_STRING, PUNICODE_STRING;
		 */
		/**
		 * The Class UNICODE_STRING.
		 */
		class UNICODE_STRING extends Structure
		{

			/** The Length. */
			public short	Length	= 0;

			/** The Maximum length. */
			public short	MaximumLength;

			/** The Buffer. */
			public Pointer	Buffer;
		}
	}

	public interface MyAdvapi extends Advapi32
	{
		MyAdvapi	INSTANCE	= (MyAdvapi) Native.loadLibrary("Advapi32", MyAdvapi.class, Options.UNICODE_OPTIONS);

		/*
		 * BOOL WINAPI LookupAccountSid( __in_opt LPCTSTR lpSystemName, __in
		 * PSID lpSid, __out_opt LPTSTR lpName, __inout LPDWORD cchName,
		 * __out_opt LPTSTR lpReferencedDomainName, __inout LPDWORD
		 * cchReferencedDomainName, __out PSID_NAME_USE peUse );
		 */
		boolean LookupAccountSidW(String lpSystemName, Pointer lpSid, Memory lpName, IntByReference cchName, Memory lpReferencedDomainName,
				IntByReference cchReferencedDomainName, IntByReference peUse);

		/*
		 * typedef struct _SID_AND_ATTRIBUTES { PSID Sid; DWORD Attributes; }
		 * SID_AND_ATTRIBUTES,PSID_AND_ATTRIBUTES;
		 */
		static class SID_AND_ATTRIBUTES extends Structure
		{
			public Pointer	Sid;
			public int		Attributes;
		}

		/*
		 * typedef struct _TOKEN_USER { SID_AND_ATTRIBUTES User; } TOKEN_USER,
		 * PTOKEN_USER;
		 */
		static class TOKEN_USER extends Structure
		{
			public SID_AND_ATTRIBUTES	User;

			public TOKEN_USER(Pointer p)
			{
				super();
				this.useMemory(p);
				this.read();
			}
		}

		public static final int	TokenPrivileges	= 3;
		public static final int	TokenUser		= 1;
		public static final int	TokenElevation	= 20;
		
		/*
		 * typedef struct _TOKEN_ELEVATION {
  DWORD TokenIsElevated;
} TOKEN_ELEVATION, *PTOKEN_ELEVATION;
		 */
		static class TOKEN_ELEVATION  extends Structure
		{
			  public int TokenIsElevated = 0;
			  
				public TOKEN_ELEVATION(Pointer p)
				{
					super();
					this.useMemory(p);
					this.read();
				}
				
				public boolean isElevated()
				{
					return TokenIsElevated != 0;
				}

			}

		/*
		 * BOOL WINAPI GetTokenInformation( __in HANDLE TokenHandle, __in
		 * TOKEN_INFORMATION_CLASS TokenInformationClass, __out_opt LPVOID
		 * TokenInformation, __in DWORD TokenInformationLength, __out PDWORD
		 * ReturnLength );
		 */
		boolean GetTokenInformation(Pointer TokenHandle, int TokenInformationClass, Memory TokenInformation, int TokenInformationLength,
				IntByReference ReturnLength);

		/*
		 * BOOL WINAPI InitializeSecurityDescriptor( __out PSECURITY_DESCRIPTOR
		 * pSecurityDescriptor, __in DWORD dwRevision );
		 */
		boolean InitializeSecurityDescriptor(Memory pSecurityDescriptor, int dwRevision);

		public static final int	SECURITY_DESCRIPTOR_MIN_LENGTH	= 20;
		public static final int	SECURITY_DESCRIPTOR_REVISION	= 1;

		/*
		 * BOOL WINAPI SetSecurityDescriptorSacl( __inout PSECURITY_DESCRIPTOR
		 * pSecurityDescriptor, __in BOOL bSaclPresent, __in_opt PACL pSacl,
		 * __in BOOL bSaclDefaulted );
		 */
		boolean SetSecurityDescriptorDacl(Pointer pSecurityDescriptor, boolean bSaclPresent, Pointer pSacl, boolean bSaclDefaulted);

		public static int	SE_PRIVILEGE_ENABLED	= 2;

		/*
		 * typedef struct _LUID { DWORD LowPart; LONG HighPart; } LUID,PLUID;
		 */
		static class LUID extends Structure
		{
			public int	LowPart;
			public int	HighPart;
		}

		/*
		 * typedef struct _LUID_AND_ATTRIBUTES { LUID Luid; DWORD Attributes; }
		 * LUID_AND_ATTRIBUTES,PLUID_AND_ATTRIBUTES;
		 */
		static class LUID_AND_ATTRIBUTES extends Structure
		{
			public LUID	Luid;
			public int	Attributes;
		}

		/*
		 * typedef struct _TOKEN_PRIVILEGES { DWORD PrivilegeCount;
		 * LUID_AND_ATTRIBUTES Privileges[ANYSIZE_ARRAY]; } TOKEN_PRIVILEGES,
		 * PTOKEN_PRIVILEGES;
		 */
		static class TOKEN_PRIVILEGES extends Structure
		{
			public int						PrivilegeCount	= 1;
			public LUID_AND_ATTRIBUTES[]	Privileges		= new LUID_AND_ATTRIBUTES[1];

			public TOKEN_PRIVILEGES()
			{
				super();
				Privileges[0] = new LUID_AND_ATTRIBUTES();
			}

			public TOKEN_PRIVILEGES(Pointer p)
			{
				super();
				PrivilegeCount = p.getInt(0);
				Privileges = new LUID_AND_ATTRIBUTES[PrivilegeCount];
				this.useMemory(p);
				this.read();
			}

		}

		/*
		 * BOOL WINAPI AdjustTokenPrivileges( __in HANDLE TokenHandle, __in BOOL
		 * DisableAllPrivileges, __in_opt PTOKEN_PRIVILEGES NewState, __in DWORD
		 * BufferLength, __out_opt PTOKEN_PRIVILEGES PreviousState, __out_opt
		 * PDWORD ReturnLength );
		 */
		boolean AdjustTokenPrivileges(Pointer TokenHandle, boolean DisableAllPrivileges, TOKEN_PRIVILEGES NewState, int BufferLength,
				PointerByReference PreviousState, IntByReference ReturnLength);

		/*
		 * BOOL WINAPI LookupPrivilegeValue( __in_opt LPCTSTR lpSystemName, __in
		 * LPCTSTR lpName, __out PLUID lpLuid );
		 */
		boolean LookupPrivilegeValueA(String lpSystemName, String lpName, LUID lpLuid);

		public static final String	SE_ASSIGNPRIMARYTOKEN_NAME	= "SeAssignPrimaryTokenPrivilege";
		public static final String	SE_INCREASE_QUOTA_NAME		= "SeIncreaseQuotaPrivilege";
		public static final String	SE_DEBUG_NAME				= "SeDebugPrivilege";
		public static final String	SE_TCB_NAME					= "SeTcbPrivilege";

		/*
		 * BOOL WINAPI OpenProcessToken( __in HANDLE ProcessHandle, __in DWORD
		 * DesiredAccess, __out PHANDLE TokenHandle );
		 */
		boolean OpenProcessToken(HANDLE ProcessHandle, int DesiredAccess, PointerByReference TokenHandle);

		public static final int	STANDARD_RIGHTS_READ	= 0x20000;
		public static final int	STANDARD_RIGHTS_WRITE	= 0x20000;
		public static final int	TOKEN_QUERY				= 0x0008;
		public static final int	TOKEN_ADJUST_PRIVILEGES	= 0x0020;
		public static final int	TOKEN_ADJUST_GROUPS		= 0x0040;
		public static final int	TOKEN_ADJUST_DEFAULT	= 0x0080;
		public static final int	TOKEN_DUPLICATE			= 0x0002;
		public static final int	TOKEN_IMPERSONATE		= 0x0004;

		public static final int	TOKEN_READ				= STANDARD_RIGHTS_READ | TOKEN_QUERY;

		public static final int	TOKEN_WRITE				= STANDARD_RIGHTS_WRITE | TOKEN_ADJUST_PRIVILEGES | TOKEN_ADJUST_GROUPS
																| TOKEN_ADJUST_DEFAULT;

		/*
		 * BOOL WINAPI CreateProcessWithLogonW( __in LPCWSTR lpUsername,
		 * __in_opt LPCWSTR lpDomain, __in LPCWSTR lpPassword, __in DWORD
		 * dwLogonFlags, __in_opt LPCWSTR lpApplicationName, __inout_opt LPWSTR
		 * lpCommandLine, __in DWORD dwCreationFlags, __in_opt LPVOID
		 * lpEnvironment, __in_opt LPCWSTR lpCurrentDirectory, __in
		 * LPSTARTUPINFOW lpStartupInfo, __out LPPROCESS_INFORMATION
		 * lpProcessInfo );
		 */
		boolean CreateProcessWithLogonW(WString lpUsername, WString lpDomain, WString lpPassword, int dwLogonFlags, WString lpApplicationName,
				WString lpCommandLine, int dwCreationFlags, Pointer lpEnvironment, WString lpCurrentDirectory, Structure lpStartupInfo,
				Structure lpProcessInfo);

		public static final int	LOGON_WITH_PROFILE			= 0x00000001;
		public static final int	LOGON_NETCREDENTIALS_ONLY	= 0x00000002;

		/*
		 * BOOL LogonUser( __in LPTSTR lpszUsername, __in_opt LPTSTR lpszDomain,
		 * __in LPTSTR lpszPassword, __in DWORD dwLogonType, __in DWORD
		 * dwLogonProvider, __out PHANDLE phToken );
		 */
		boolean LogonUserA(String lpszUsername, String lpszDomain, String lpszPassword, int dwLogonType, int dwLogonProvider,
				PointerByReference phToken);

		boolean LogonUserW(WString lpszUsername, WString lpszDomain, WString lpszPassword, int dwLogonType, int dwLogonProvider,
				PointerByReference phToken);

		public static final int	LOGON32_LOGON_INTERACTIVE		= 2;
		public static final int	LOGON32_LOGON_NETWORK			= 3;
		public static final int	LOGON32_LOGON_BATCH				= 4;
		public static final int	LOGON32_LOGON_SERVICE			= 5;
		public static final int	LOGON32_LOGON_UNLOCK			= 7;
		public static final int	LOGON32_LOGON_NETWORK_CLEARTEXT	= 8;
		public static final int	LOGON32_LOGON_NEW_CREDENTIALS	= 9;

		public static final int	LOGON32_PROVIDER_DEFAULT		= 0;
		public static final int	LOGON32_PROVIDER_WINNT35		= 1;
		public static final int	LOGON32_PROVIDER_WINNT40		= 2;
		public static final int	LOGON32_PROVIDER_WINNT50		= 3;

		/*
		 * BOOL WINAPI ImpersonateLoggedOnUser( __in HANDLE hToken );
		 */
		boolean ImpersonateLoggedOnUser(Pointer hToken);

		/*
		 * BOOL WINAPI CreateProcessAsUser( __in_opt HANDLE hToken, __in_opt
		 * LPCTSTR lpApplicationName, __inout_opt LPTSTR lpCommandLine, __in_opt
		 * LPSECURITY_ATTRIBUTES lpProcessAttributes, __in_opt
		 * LPSECURITY_ATTRIBUTES lpThreadAttributes, __in BOOL bInheritHandles,
		 * __in DWORD dwCreationFlags, __in_opt LPVOID lpEnvironment, __in_opt
		 * LPCTSTR lpCurrentDirectory, __in LPSTARTUPINFO lpStartupInfo, __out
		 * LPPROCESS_INFORMATION lpProcessInformation );
		 */
		boolean CreateProcessAsUserW(Pointer hToken, WString lpApplicationName, WString lpCommandLine, Structure lpProcessAttributes,
				Structure lpThreadAttributes, boolean bInheritHandles, int dwCreationFlags, Structure lpEnvironment, WString lpCurrentDirectory,
				Structure lpStartupInfo, Structure lpProcessInformation);

		static class SECURITY_ATTRIBUTES
		{
			public int		nLength;
			public Pointer	lpSecurityDescriptor;
			boolean			bInheritHandle;
		}

		/*
		 * BOOL WINAPI DuplicateTokenEx( __in HANDLE hExistingToken, __in DWORD
		 * dwDesiredAccess, __in_opt LPSECURITY_ATTRIBUTES lpTokenAttributes,
		 * __in SECURITY_IMPERSONATION_LEVEL ImpersonationLevel, __in TOKEN_TYPE
		 * TokenType, __out PHANDLE phNewToken );
		 */
		boolean DuplicateTokenEx(Pointer hExistingToken, int dwDesiredAccess, Pointer lpTokenAttributes, int ImpersonationLevel, int TokenType,
				PointerByReference phNewToken);

	}

	public interface Secur32 extends StdCallLibrary
	{

		/** The INSTANCE. */
		Secur32	INSTANCE	= (Secur32) Native.loadLibrary("Secur32", Secur32.class);

		/*
		 * BOOLEAN WINAPI GetUserNameEx( __in EXTENDED_NAME_FORMAT NameFormat,
		 * __out LPTSTR lpNameBuffer, __inout PULONG lpnSize );
		 */
		boolean GetUserNameEx(int NameFormat, Memory lpNameBuffer, IntByReference lpnSize);

	}

	public interface MyWtsapi32 extends StdCallLibrary
	{

		// Method declarations, constant and structure definitions go here

		/** The INSTANCE. */
		MyWtsapi32	INSTANCE	= (MyWtsapi32) Native.loadLibrary("Wtsapi32", MyWtsapi32.class);

		/*
		 * BOOL WTSQueryUserToken( __in ULONG SessionId, __out PHANDLE phToken);
		 */
		boolean WTSQueryUserToken(int SessionId, PointerByReference phToken);

	}

	/** The _startup info. */
	volatile STARTUPINFO						_startupInfo;

	/** The _process information. */
	volatile PROCESS_INFORMATION	_processInformation;

	/** The in read. */
	volatile PointerByReference				inRead		= null;

	/** The in write. */
	volatile PointerByReference				inWrite		= null;

	/** The out read. */
	volatile PointerByReference				outRead		= null;

	/** The out write. */
	volatile PointerByReference				outWrite	= null;

	/** The err read. */
	volatile PointerByReference				errRead		= null;

	/** The err write. */
	volatile PointerByReference				errWrite	= null;

	/** The sa. */
	volatile SECURITY_ATTRIBUTES				sa;

	/** The m_h out pipe. */
	volatile Pointer							m_hOutPipe	= null;

	/** The m_h err pipe. */
	volatile Pointer							m_hErrPipe	= null;

	/** The m_h in pipe. */
	volatile Pointer							m_hInPipe	= null;

	/** The in write pipe. */
	volatile Pointer							inWritePipe	= null;

	/** The out read pipe. */
	volatile Pointer							outReadPipe	= null;

	/** The err read pipe. */
	volatile Pointer							errReadPipe	= null;
	
	volatile int _isElevated = -1; // 1 = true, 0 = false;

	/**
	 * Gets the process.
	 * 
	 * @param pid
	 *            the pid
	 * 
	 * @return the process
	 */
	public static Process getProcess(int pid)
	{
		WindowsXPProcess result = new WindowsXPProcess();
		HANDLE hProcess = MyKernel32.INSTANCE.OpenProcess(MyKernel32.PROCESS_ALL_ACCESS, false, pid);
		if (hProcess == null)
			hProcess = MyKernel32.INSTANCE.OpenProcess(MyKernel32.PROCESS_QUERY_INFORMATION, false, pid);
		if (hProcess == null)
			return null;

		result._pid = pid;
		result._processInformation = new PROCESS_INFORMATION();
		result._processInformation.dwProcessId = pid;
		result._processInformation.hProcess = hProcess;
		result._cmd = result.getCommandLineInternal();
		// this does not always work (why ??), if so try again, then this
		// normally does
		// on win64 PEB of 64 bit cannot be accessed from wow -> use wmi
		if (result._cmd.equals("?"))
			result._cmd = result.getCommandLineInternalWMI();
		if ("?".equals(result._cmd))
		{
			System.err.println("Could not get commandline");
		}
		//else
		//	System.out.println("Command line of " + pid + ": " + result._cmd);
		PointerByReference hToken = new PointerByReference();
		HANDLE hp = new HANDLE();
		hp.setPointer(hProcess.getPointer());
		if (MyAdvapi.INSTANCE.OpenProcessToken(hp, MyAdvapi.TOKEN_READ, hToken))
		{
			IntByReference dwSize = new IntByReference();
			MyAdvapi.INSTANCE.GetTokenInformation(hToken.getValue(), MyAdvapi.TokenUser, null, 0, dwSize);
			{
				Memory pTokenUser = new Memory(dwSize.getValue());
				if (MyAdvapi.INSTANCE.GetTokenInformation(hToken.getValue(), MyAdvapi.TokenUser, pTokenUser, dwSize.getValue(), dwSize))
				{
					MyAdvapi.TOKEN_USER tokenUser = new MyAdvapi.TOKEN_USER(pTokenUser);
					Pointer lpSid = tokenUser.User.Sid;
					Memory lpName = new Memory(256);
					IntByReference cchName = new IntByReference();
					cchName.setValue(256);
					Memory lpReferencedDomainName = new Memory(256);
					IntByReference cchReferencedDomainName = new IntByReference();
					cchReferencedDomainName.setValue(256);
					IntByReference peUse = new IntByReference();
					if (MyAdvapi.INSTANCE.LookupAccountSidW(null, lpSid, lpName, cchName, lpReferencedDomainName, cchReferencedDomainName, peUse))

						result._user = lpReferencedDomainName.getString(0, true) + "\\" + lpName.getString(0, true);
					;
					// System.out.println(result._user);
				}
			}
			if (result._user == null)
				System.out.println("could not get user name OS error #" + MyKernel32.INSTANCE.GetLastError());
			MyKernel32.INSTANCE.CloseHandle(hToken.getValue());
		}
		return result;
	}

	private boolean setPrivilege(Pointer hToken, String lpszPrivilege, boolean bEnablePrivilege)
	{
		TOKEN_PRIVILEGES tp = new TOKEN_PRIVILEGES();
		MyAdvapi.LUID luid = new MyAdvapi.LUID();
		luid.size();

		if (!MyAdvapi.INSTANCE.LookupPrivilegeValueA(null, lpszPrivilege, luid))
			return false;

		tp.Privileges[0].Luid = luid;
		tp.write();

		if (bEnablePrivilege)
			tp.Privileges[0].Attributes = MyAdvapi.SE_PRIVILEGE_ENABLED;
		else
			tp.Privileges[0].Attributes = 0;

		int size = tp.size();
		boolean result = MyAdvapi.INSTANCE.AdjustTokenPrivileges(hToken, false, tp, 0, null, null);
		// return GetLastError() == ERROR_SUCCESS;
		if (!result)
		{
			int errNr = MyKernel32.INSTANCE.GetLastError();
			log("error setting privliges OS error #" + errNr + "/" + Integer.toHexString(errNr));
		}

		return result;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.os.Process#start()
	 */
	public boolean start()
	{
		boolean result = false;

		if (isRunning())
		{
			log("process already running -> abort start");
			return false;
		}
		else
		{
			setPid(-1);
			setExitCode(-1);
		}
		_started = false;

		int PIPE_SIZE = 1024; // buffer size for pipes
		int PIPE_TIMEOUT = 12000; // time to wait for pipe

		if (_arrCmd == null && _cmd == null)
			return false;
		if (_cmd == null)
		{
			_cmd = "";
			for (String cmd : _arrCmd)
			{
				if (cmd == null || cmd.length() == 0)
					continue;
				if (cmd.contains(" ") && !cmd.endsWith("\""))
						_cmd += '"' + cmd + "\" ";
				else
					_cmd += cmd + " ";
			}
			// _cmd += cmd + " ";
		}
		if (_debug)
			log("exec: " + _cmd);
		if (_processInformation != null)
		{
			log("process not correctly disposed -> abort start");
			return false;
		}
		try
		{
			destroyed = false;
			_startupInfo = new STARTUPINFO();
			_startupInfo.clear();
			_processInformation = new PROCESS_INFORMATION();
			_processInformation.clear();
			if (_pipeStreams)
			{
				if (sa == null)
				{
					sa = new SECURITY_ATTRIBUTES();
					sa.clear();
					sa.nLength = sa.size();
					sa.lpSecurityDescriptor = null;
					sa.bInheritHandle = true;// 1; // true otherwise streams are
					// not piped
				}
				inRead = new PointerByReference();
				inWrite = new PointerByReference();
				outRead = new PointerByReference();
				outWrite = new PointerByReference();
				errRead = new PointerByReference();
				errWrite = new PointerByReference();


				_startupInfo.dwFlags = MyKernel32.STARTF_USESTDHANDLES;
				
				if (MyKernel32.INSTANCE.CreatePipe(inRead, inWrite, sa, 0) == 0)
				{
					log("Error in CreatePipe inWrite " + Integer.toHexString(MyKernel32.INSTANCE.GetLastError()));
					return false;					
				}
				if (!MyKernel32.INSTANCE.SetHandleInformation(inWrite.getValue(), MyKernel32.HANDLE_FLAG_INHERIT, 0))
				{
					log("error in set handle inWrite -> abort start");
					return false;					
				}
				_startupInfo.hStdInput = inRead.getValue();

				if (MyKernel32.INSTANCE.CreatePipe(outRead, outWrite, sa, 0) == 0)
				{
					log("Error in CreatePipe outWrite " + Integer.toHexString(MyKernel32.INSTANCE.GetLastError()));
					return false;					
				}
				if (!MyKernel32.INSTANCE.SetHandleInformation(outRead.getValue(), MyKernel32.HANDLE_FLAG_INHERIT, 0))
				{
					log("error in set handle outRead -> abort start");
					return false;					
				}
				_startupInfo.hStdOutput = outWrite.getValue();

				if (MyKernel32.INSTANCE.CreatePipe(errRead, errWrite, sa, 0) == 0)
				{
					log("Error in CreatePipe errWrite " + Integer.toHexString(MyKernel32.INSTANCE.GetLastError()));
					return false;					
				}
				if (!MyKernel32.INSTANCE.SetHandleInformation(errRead.getValue(), MyKernel32.HANDLE_FLAG_INHERIT, 0))
				{
					log("error in set handle inWrite -> abort start");
					return false;					
				}
				_startupInfo.hStdError = errWrite.getValue();
				
						// for some unknown reason: if we add the following
						// lines we do not get "operation on non socket" error
						// in mina
						/*|| !MyKernel32.INSTANCE.SetHandleInformation(inWrite.getValue(), MyKernel32.HANDLE_FLAG_PROTECT_FROM_CLOSE,
								MyKernel32.HANDLE_FLAG_PROTECT_FROM_CLOSE)
						|| !MyKernel32.INSTANCE.SetHandleInformation(outRead.getValue(), MyKernel32.HANDLE_FLAG_PROTECT_FROM_CLOSE,
								MyKernel32.HANDLE_FLAG_PROTECT_FROM_CLOSE)
						|| !MyKernel32.INSTANCE.SetHandleInformation(errRead.getValue(), MyKernel32.HANDLE_FLAG_PROTECT_FROM_CLOSE,
								MyKernel32.HANDLE_FLAG_PROTECT_FROM_CLOSE)
								*/
								
				if (this._redirectErrorStream)
					MyKernel32.INSTANCE.SetHandleInformation(errRead.getValue(), MyKernel32.HANDLE_FLAG_INHERIT, 0);

			}

			int creationFlag = 0;
			if (!_visible)
			{
				creationFlag |= MyKernel32.CREATE_NO_WINDOW | MyKernel32.CREATE_UNICODE_ENVIRONMENT;
				_startupInfo.lpTitle = null;
			}
			else
			{
				creationFlag |= MyKernel32.CREATE_NEW_CONSOLE | MyKernel32.CREATE_UNICODE_ENVIRONMENT;
				_startupInfo.lpTitle = new WString(_title);

				if (_minimized)
				{
					_startupInfo.wShowWindow |= MyKernel32.SW_SHOWMINIMIZED | MyKernel32.SW_SHOWNOACTIVATE;
					_startupInfo.dwFlags |= MyKernel32.STARTF_USESHOWWINDOW;
				}

			}

			creationFlag |= getPriorityFlag();

			// do not inherit handles. otherwise resources are not freed if
			// parent is killed
			// inherit only when we need to pipe the streams
			_startupInfo.write();
			WString cmd = new WString(_cmd);
			WString wDir = getWorkingDir() == null ? null : new WString(getWorkingDir());
			String stdUser = standardizeUser(_user);
			StringBlock environment = null;
			WString[] env = null;
			if (_environment.size() != 0)
			{
				env = new WString[_environment.size()];
				int i = 0;
				for (String[] entry : _environment)
				{
					env[i++] = new WString(entry[0] + "=" + entry[1]);
				}
				environment = new StringBlock(env);
			}
			if (_desktop != null)
			{
				_startupInfo.lpDesktop = new WString(_desktop);
				log("setting desktop "+_desktop);
			}

			 if (_logonActiveSession)
				{
					log("start process in active session");
					int session = 0xFFFFFFFF;
					// wait until we have an active session
					while (session == 0xFFFFFFFF)
					{
						session = MyKernel32.INSTANCE.WTSGetActiveConsoleSessionId();
						if (session == 0xFFFFFFFF)
							Thread.sleep(1000);
						log("active session: "+session);
					}

					
					PointerByReference phToken = new PointerByReference();
					
					// wait for a user to log on to the session
					boolean userLoggedOn = false;
					int retries = 0;
					while (!userLoggedOn)
					{
					result = MyWtsapi32.INSTANCE.WTSQueryUserToken(session, phToken);
					userLoggedOn = result || MyKernel32.INSTANCE.GetLastError() != 1008;
					if (!userLoggedOn)
					{
						Thread.sleep(1000);
						retries++;
					}
					}
					// if user just logged on: wait for the desktop to get up.
					// TODO evntl. add a configuration property for the time to wait.
					if (retries > 0)
						Thread.sleep(10000);
					
					if (!doesUserHavePrivilege(MyAdvapi.SE_TCB_NAME))
						log("WARNING: Process does not have the SE_TCB_NAME privilege !!");

					// start the application
					if (result)
					{
						log("got session token: "+phToken.getValue());
						PointerByReference phNewToken = new PointerByReference();
						result = MyAdvapi.INSTANCE.DuplicateTokenEx(phToken.getValue(), 0x2000000, null, 0, 1, phNewToken);
						if (result)
						{
							log("duplicated token: "+phNewToken.getValue());
							//_startupInfo.lpDesktop = new WString("winsta0\\default");
							creationFlag = 0;
							creationFlag |= MyKernel32.CREATE_NO_WINDOW | MyKernel32.CREATE_UNICODE_ENVIRONMENT;
							creationFlag |= getPriorityFlag();
							result = MyAdvapi.INSTANCE.CreateProcessAsUserW(phNewToken.getValue(), null, cmd, null, null, _pipeStreams, creationFlag,
									null, new WString(getWorkingDir()), _startupInfo, _processInformation);
							log("started "+result);
						}
					}

				}
			 else
			if (stdUser == null || stdUser.equals(currentUser()))
			{
				result = MyKernel32.INSTANCE.CreateProcessW(null, cmd, null, null, _pipeStreams, creationFlag, environment, wDir, _startupInfo,
						_processInformation);
			}
			else
			{
				WString user = null;
				
				WString domain = null;
				
				int i = _user.lastIndexOf("\\");
				if (i > 0)
				{
					user = new WString(_user.substring(_user.lastIndexOf("\\") + 1));
					domain = new WString(_user.substring(0, _user.lastIndexOf("\\")));
				}
				else
					user = new WString(_user);
				WString password = null;
				if (_password != null)
					password = new WString(_password);

				log("current user :: requested user: " + currentUserName() + " :: " + stdUser);
				// in windows 2008: system user seems to be <computername>$
				// could not find documentation on this.
				if (!("SYSTEM".equals(currentUserName()) || currentUserName().endsWith("$")))
				{
					// createProcessWithLogon : cmd line is only 1024 char long
					// parent process is not current process.
					// -> use CreateProcessAsUser
					// result = MyAdvapi.INSTANCE.CreateProcessWithLogonW(user,
					// domain, password, MyAdvapi.LOGON_WITH_PROFILE, null, cmd,
					// creationFlag, null, wDir, _startupInfo,
					// _processInformation);

					/**/
					PointerByReference phToken = new PointerByReference();

					String stUser = user.toString();
					String stDomain = domain == null ? null : domain.toString();
					String stPassword = password == null ? "" : password.toString();
					result = true;
					// result = MyAdvapi.INSTANCE.LogonUserA(stUser, stDomain,
					// stPassword, MyAdvapi.LOGON32_LOGON_NEW_CREDENTIALS,
					// MyAdvapi.LOGON32_PROVIDER_WINNT50, phToken);
					if (result)
					{
						// HANDLE hCurrentProcess =
						// MyKernel32.INSTANCE.GetCurrentProcess();
						// PointerByReference hTokenSelf = new
						// PointerByReference();
						// result =
						// MyAdvapi.INSTANCE.OpenProcessToken(hCurrentProcess,
						// MyAdvapi.TOKEN_READ | MyAdvapi.TOKEN_WRITE |
						// MyAdvapi.TOKEN_DUPLICATE |
						// MyAdvapi.TOKEN_IMPERSONATE, hTokenSelf );
						/*
						 * Memory pSD = new
						 * Memory(MyAdvapi.SECURITY_DESCRIPTOR_MIN_LENGTH);
						 * pSD.clear(); if (result) result =
						 * MyAdvapi.INSTANCE.InitializeSecurityDescriptor(pSD,
						 * MyAdvapi.SECURITY_DESCRIPTOR_REVISION);
						 * 
						 * if (result) result =
						 * MyAdvapi.INSTANCE.SetSecurityDescriptorDacl(pSD,
						 * true, null, false);
						 */
						if (result)
						{

							/*
							 * SECURITY_ATTRIBUTES sap = new
							 * SECURITY_ATTRIBUTES(); sap.clear(); sap.nLength =
							 * sap.size(); sap.lpSecurityDescriptor = pSD;
							 * sap.bInheritHandle = false;
							 */
							// result =
							// MyAdvapi.INSTANCE.ImpersonateLoggedOnUser(phToken.getValue());
							/**/// System.out.println(MyAdvapi.SE_ASSIGNPRIMARYTOKEN_NAME+" "+this.doesUserHavePrivilege(MyAdvapi.SE_ASSIGNPRIMARYTOKEN_NAME));
							// System.out.println(MyAdvapi.SE_INCREASE_QUOTA_NAME+" "+this.doesUserHavePrivilege(MyAdvapi.SE_INCREASE_QUOTA_NAME));
							// System.out.println(MyAdvapi.SE_DEBUG_NAME+" "+this.doesUserHavePrivilege(MyAdvapi.SE_DEBUG_NAME));
							// System.out.println(MyAdvapi.SE_TCB_NAME+" "+this.doesUserHavePrivilege(MyAdvapi.SE_TCB_NAME));
							// */
							/**/if (result)
							{
								// result =
								// setPrivilege(hTokenSelf.getValue(),
								// MyAdvapi.SE_ASSIGNPRIMARYTOKEN_NAME, true)
								// && setPrivilege(hTokenSelf.getValue(),
								// MyAdvapi.SE_INCREASE_QUOTA_NAME, true)
								// && setPrivilege(hTokenSelf.getValue(),
								// MyAdvapi.SE_DEBUG_NAME, true)
								// && setPrivilege(hTokenSelf.getValue(),
								// MyAdvapi.SE_TCB_NAME, true)
								;
							}
							// System.out.println(MyAdvapi.SE_ASSIGNPRIMARYTOKEN_NAME+" "+this.doesUserHavePrivilege(MyAdvapi.SE_ASSIGNPRIMARYTOKEN_NAME));
							// System.out.println(MyAdvapi.SE_INCREASE_QUOTA_NAME+" "+this.doesUserHavePrivilege(MyAdvapi.SE_INCREASE_QUOTA_NAME));
							// System.out.println(MyAdvapi.SE_DEBUG_NAME+" "+this.doesUserHavePrivilege(MyAdvapi.SE_DEBUG_NAME));
							// System.out.println(MyAdvapi.SE_TCB_NAME+" "+this.doesUserHavePrivilege(MyAdvapi.SE_TCB_NAME));
							// */
							// MyKernel32.INSTANCE.CloseHandle(hTokenSelf.getValue());
							if (!doesUserHavePrivilege(MyAdvapi.SE_ASSIGNPRIMARYTOKEN_NAME))
								log("Process does not have the SE_ASSIGNPRIMARYTOKEN_NAME privilege !!");

							if (!doesUserHavePrivilege(MyAdvapi.SE_INCREASE_QUOTA_NAME))
								log("Process does not have the SE_INCREASE_QUOTA_NAME privilege !!");

							result = MyAdvapi.INSTANCE.LogonUserA(stUser, stDomain, stPassword, MyAdvapi.LOGON32_LOGON_INTERACTIVE,
									MyAdvapi.LOGON32_PROVIDER_DEFAULT, phToken);
							if (result)
								// result =
								// MyAdvapi.INSTANCE.CreateProcessWithLogonW(user,
								// domain, password,
								// MyAdvapi.LOGON_NETCREDENTIALS_ONLY, null,
								// cmd, creationFlag, null, wDir, _startupInfo,
								// _processInformation);
								result = MyAdvapi.INSTANCE.CreateProcessAsUserW(phToken.getValue(), null, cmd, null, // sap,
										null, true,// _pipeStreams,
										creationFlag, null, null,// getWorkingDir(),
										_startupInfo, _processInformation);
						}

					}
					/**/

				}
				else
				{
					/*
					 * _startupInfo = new STARTUPINFO(); _startupInfo.clear();
					 * _processInformation = new PROCESS_INFORMATION();
					 * _processInformation.clear();
					 */
					PointerByReference phToken = new PointerByReference();

					String stUser = user.toString();
					String stDomain = domain == null ? null : domain.toString();
					String stPassword = password == null ? "" : password.toString();

					result = MyAdvapi.INSTANCE.LogonUserW(user, domain, password, MyAdvapi.LOGON32_LOGON_INTERACTIVE,
							MyAdvapi.LOGON32_PROVIDER_DEFAULT, phToken);
					log("logonUserA " + result);
					if (result)
					{
						// result =
						// MyAdvapi.INSTANCE.ImpersonateLoggedOnUser(phToken.getValue());
						if (result)
							// result =
							// MyAdvapi.INSTANCE.CreateProcessWithLogonW(user,
							// domain, password,
							// MyAdvapi.LOGON_NETCREDENTIALS_ONLY, null, cmd,
							// creationFlag, null, wDir, _startupInfo,
							// _processInformation);
							result = MyAdvapi.INSTANCE.CreateProcessAsUserW(phToken.getValue(), null, cmd, null, null, _pipeStreams, creationFlag,
									null, new WString(getWorkingDir()), _startupInfo, _processInformation);

					}

				}
			}

			if (!result)
			{
				int err = MyKernel32.INSTANCE.GetLastError();
				log("could not start process " + Integer.toHexString(err));
				log(Kernel32Util.formatMessageFromLastErrorCode(err));
				log(_startupInfo.toString());
				return result;
			}
			_started = true;

			// Thread.sleep(1000);

			int res = MyUser32.INSTANCE.WaitForInputIdle(_processInformation.hProcess, 2000);
			if (res > 0)
			{
				log("Warning: WaitForInputIdle returned " + res);
				// return false;
			}

			int affinity = getProcessAffinity();
			if (affinity > 0)
			{
				if (!MyKernel32.INSTANCE.SetProcessAffinityMask(_processInformation.hProcess, affinity))
					log("could not set process affinity");
			}

			if (_pipeStreams)
			{

				// Thread.sleep(15000);
				/*
				 * Memory buf = new Memory(1); IntByReference rres = new
				 * IntByReference(); System.out.println("readddd"); boolean x =
				 * MyKernel32.INSTANCE.ReadFile(outRead.getValue(), buf, (int)
				 * buf.getSize(), rres, null); if (!x)
				 * System.out.println("read error"); else
				 * System.out.println(buf.getByte(0));
				 */

				writefd(in_fd, inWrite.getValue());
				writefd(out_fd, outRead.getValue());
				writefd(err_fd, errRead.getValue());

				_outputStream = new BufferedOutputStream(new FileOutputStream(in_fd));
				_inputStream = new BufferedInputStream(new FileInputStream(out_fd));
				_errorStream = new BufferedInputStream(new FileInputStream(err_fd));

				MyKernel32.INSTANCE.CloseHandle(inRead.getValue());
				MyKernel32.INSTANCE.CloseHandle(outWrite.getValue());
				MyKernel32.INSTANCE.CloseHandle(errWrite.getValue());

			}
			else if (_teeName != null && _tmpPath != null)
			{
				File f = new File(_tmpPath);
				if (!f.exists())
					f.mkdir();
				_outputStream = new CyclicBufferFilePrintStream(new File(_tmpPath, "in_" + _teeName));
				_inputStream = new CyclicBufferFileInputStream(new File(_tmpPath, "out_" + _teeName));
				_errorStream = new CyclicBufferFileInputStream(new File(_tmpPath, "err_" + _teeName));
				new File(_tmpPath, "in_" + _teeName).deleteOnExit();
				new File(_tmpPath, "out_" + _teeName).deleteOnExit();
				new File(_tmpPath, "err_" + _teeName).deleteOnExit();
			}

			_pid = _processInformation.dwProcessId;
		}
		catch (Exception ex)
		{
			log("exception in process start: " + ex);
			ex.printStackTrace();
		}

		return result;
	}

	/**
	 * Gets the process affinity.
	 * 
	 * @return the process affinity
	 */
	private int getProcessAffinity()
	{
		if (_cpuAffinity <= 0)
			return 0;
		IntByReference lpProcessAffinityMask = new IntByReference();
		IntByReference lpSystemAffinityMask = new IntByReference();
		if (MyKernel32.INSTANCE.GetProcessAffinityMask(_processInformation.hProcess, lpProcessAffinityMask, lpSystemAffinityMask))
			return lpSystemAffinityMask.getValue() & _cpuAffinity;
		else
		{
			log("could not get process affinity mask -> not setting");
			return 0;
		}
	}

	/**
	 * Gets the priority flag.
	 * 
	 * @return the priority flag
	 */
	private int getPriorityFlag()
	{
		switch (_priority)
		{
		case PRIORITY_NORMAL:
			return MyKernel32.NORMAL_PRIORITY_CLASS;
		case PRIORITY_ABOVE_NORMAL:
			return MyKernel32.ABOVE_NORMAL_PRIORITY_CLASS;
		case PRIORITY_HIGH:
			return MyKernel32.HIGH_PRIORITY_CLASS;
		case PRIORITY_BELOW_NORMAL:
			return MyKernel32.BELOW_NORMAL_PRIORITY_CLASS;
		case PRIORITY_LOW:
			return MyKernel32.IDLE_PRIORITY_CLASS;
		default:
			return 0;
		}
	}

	// fd.handle = pointer.peer, using reflection, since both are private
	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.os.Process#waitFor()
	 */
	public void waitFor()
	{
		if (!isRunning())
			return;
		waitFor(MyKernel32.INFINITE);
	}

	/**
	 * Gets the exit code internal.
	 * 
	 * @return the exit code internal
	 */
	private int getExitCodeInternal()
	{
		IntByReference code = new IntByReference();
		if (_processInformation == null)
			return -1;
		boolean result = MyKernel32.INSTANCE.GetExitCodeProcess(_processInformation.hProcess, code);
		try
		{
			// if server overloaded windows may need some time to set the exit
			// code.
			Thread.sleep(100);
		}
		catch (InterruptedException e)
		{
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		// System.out.println("get exit code internal " + result + " " +
		// code.getValue());
		if (result)
		{
			if (_debug)
				log("GetExitCodeProcess returned " + code.getValue());
			return code.getValue();
		}
		else
		{
			log("Error in GetExitCodeProcess OS Error #" + MyKernel32.INSTANCE.GetLastError());
			return -3;
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.os.Process#waitFor(int)
	 */
	public void waitFor(long timeout)
	{
		if (_debug)
			log("waitFor "+timeout);
		try
		{
		if (!isRunning())
			return;
		if (_debug)
			log("1waitFor ");
		if (timeout > Integer.MAX_VALUE)
			timeout = Integer.MAX_VALUE;
		if (_processInformation == null)
			return;
		long start = System.currentTimeMillis();
		if (_debug)
			log("2waitFor ");
		while (_processInformation != null && (timeout == -1 || (start+timeout) > System.currentTimeMillis()) && isRunning())
		{
			if (_debug)
				log("WaitForSingleObject +");
		int result = MyKernel32.INSTANCE.WaitForSingleObject(_processInformation.hProcess, (int) timeout);
		if (_debug)
			log("WaitForSingleObject -");
		if (_debug)
			log("WaitForSingleObject terminated PID: " + getPid());
		if (result == MyKernel32.WAIT_FAILED)
		{
			int errNr = MyKernel32.INSTANCE.GetLastError();
			log("Error in Process.waitFor OS Error #" + errNr + " " + Kernel32Util.formatMessageFromLastErrorCode(errNr));
		}
		else if (result != MyKernel32.WAIT_OBJECT_0)
		{
			log("Error in Process.waitFor OS result #" + result + " " + Kernel32Util.formatMessageFromLastErrorCode(result));
		}
		}
		}
		catch (Throwable ex)
		{
			log("Exception in Process.waitFor: "+ ex);
		}

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.os.Process#stop(int, int)
	 */
	public boolean stop(int timeout, int code)
	{
		if (_pid <= 0)
		{
			log("cannot kill process with negative pid " + _pid);
			return false;
		}
		// first try polite kill
		// e.g. post WM_CLOSE to all windows whose PID
		// matches our process.
		MyUser32.WNDENUMPROC closeWindow = new MyUser32.WNDENUMPROC()
		{
			// lParam is the pid of our process
			public boolean callback(HWND wnd, int lParam)
			{
				// get the pid of the window
				IntByReference dwID = new IntByReference();
				MyUser32.INSTANCE.GetWindowThreadProcessId(wnd, dwID);
				// if this windows belongs to our process
				if (dwID.getValue() == lParam)
				{
					// System.out.println("post message a: " + wnd);
					MyUser32.INSTANCE.PostMessageA(wnd, MyUser32.WM_CLOSE, null, null);
					// MyUser32.INSTANCE.PostMessageA(wnd, MyUser32.WM_QUIT,
					// null, null) ;
					// MyUser32.INSTANCE.PostMessageA(wnd, MyUser32.WM_DESTROY,
					// null, null) ;
				}
				// continue with next window
				return true;
			}
		};
		// execute closeWindow on all windows
		MyUser32.INSTANCE.EnumWindows(closeWindow, _pid);
		try
		{
			Thread.sleep(100);
		}
		catch (InterruptedException e)
		{
			e.printStackTrace();
			Thread.currentThread().interrupt();
		}

		// Wait for process to terminate

		if (timeout > 0)
			waitFor(timeout);

		// give system time to put exit code
		try
		{
			Thread.sleep(100);
		}
		catch (InterruptedException e)
		{
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		// If still running -> hard kill
		if (isRunning())
		{
			log("process is not polite -> hard kill");
			return kill(code);
		}
		else
		{
			// _processInformation = null;
			_pid = -1;
			setExitCode(code);
			return true;
		}
	}

	// public boolean cleanKill(int code, int timeout)
	// {
	// // first try polite kill
	// // e.g. post WM_CLOSE to all windows whose PID
	// // matches our process.
	// MyUser32.WNDENUMPROC closeWindow = new MyUser32.WNDENUMPROC()
	// {
	// // lParam is the pid of our process
	// public boolean callback(Pointer wnd, int lParam)
	// {
	// // get the pid of the window
	// IntByReference dwID = new IntByReference();
	// MyUser32.INSTANCE.GetWindowThreadProcessId(wnd, dwID) ;
	// // if this windows belongs to our process
	// if(dwID.getValue() == lParam)
	// {
	// MyUser32.INSTANCE.PostMessageA(wnd, MyUser32.WM_CLOSE, null, null) ;
	// }
	// // continue with next window
	// return true;
	// }
	// };
	// // execute closeWindow on all windows
	// MyUser32.INSTANCE.EnumWindows(closeWindow ,
	// _pid) ;
	//
	// // Wait for process to terminate
	// waitFor(timeout);
	// // If still running -> hard kill
	// if (isRunning())
	// return kill(code);
	// return false;
	//
	// }

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.os.Process#kill(int)
	 */
	public boolean kill(int code)
	{
		boolean result = false;
		try
		{
			if (_pid <= 0)
			{
				log("cannot kill process with pid " + _pid);
				return false;
			}

			if (!isRunning())
			{
				// _processInformation = null;
				_pid = -1;
				return false;
			}
			int i = 0;
			if (_processInformation != null && _processInformation.hProcess != null)
				while (!result && i < 10)
				{
					if (_processInformation != null && _processInformation.hProcess != null)
					{
						result = MyKernel32.INSTANCE.TerminateProcess(_processInformation.hProcess, code);
						if (!result)
						{
							log("kill of process with PID " + _pid + " failed: OS Error #" + MyKernel32.INSTANCE.GetLastError());
							i++;
							try
							{
								Thread.sleep(500);
							}
							catch (InterruptedException e)
							{
								e.printStackTrace();
								Thread.currentThread().interrupt();
							}

						}
					}
					else
					{
						Thread.sleep(1000);
						result = !isRunning();
					}

				}
			Thread.sleep(100);
			if (!isRunning())
				_pid = -1;
		}
		catch (Exception ex)
		{
			ex.printStackTrace();
			result = true;
		}

		if (!result)
			log("kill failed: " + _pid + " process still running");

		return result;
	}

	/**
	 * Kill.
	 * 
	 * @param pid
	 *            the pid
	 * @param code
	 *            the code
	 * 
	 * @return true, if successful
	 */
	public static boolean kill(int pid, int code)
	{
		if (pid <= 0)
			return false;
		HANDLE hProcess = MyKernel32.INSTANCE.OpenProcess(MyKernel32.PROCESS_TERMINATE, false, pid);
		boolean result = MyKernel32.INSTANCE.TerminateProcess(hProcess, code);
		Thread.yield();
		if (!result)
			System.out.println("process kill failed: " + pid + " code=" + code);
		MyKernel32.INSTANCE.CloseHandle(hProcess);
		return result;
	}

	/**
	 * Gets the process maps.
	 * 
	 * @param pid
	 *            the pid
	 * 
	 * @return the process maps
	 */
	public static Map[] getProcessMaps(int pid)
	{
		Map processMap = new HashMap();
		Map childrenMap = new MultiHashMap();
		Map[] result = new Map[]
		{ processMap, childrenMap };

		Pointer processes = MyKernel32.INSTANCE.CreateToolhelp32Snapshot(MyKernel32.TH32CS_SNAPPROCESS, 0);
		if (processes == null)
		{
			System.out.println("note: task list is empty ");
			return result;
		}

		PROCESSENTRY32 me = new PROCESSENTRY32();
		me.szExeFile = new char[MyKernel32.MAX_PATH];
		int size = me.size();
		// System.out.println("size: " + size);
		me.dwSize = size;
		if (MyKernel32.INSTANCE.Process32First(processes, me))
		{
			System.out.println("ProcessList:");
			do
			{
				// System.out.println(/* new String(next.szExeFile) + */" " +
				// me.th32ModuleID + " " + me.th32DefaultHeapID + " " +
				// me.th32ProcessID
				// + " -> " + me.th32ParentProcessID);
				if (me.th32ProcessID > 0)
					processMap.put(new Integer(me.th32ProcessID), me);
				if (me.th32ParentProcessID > 0 && processMap.get(new Integer(me.th32ParentProcessID)) != null)
				{
					childrenMap.put(new Integer(me.th32ParentProcessID), new Integer(me.th32ProcessID));
				}
				System.out.println("\tProcessID=" + me.th32ProcessID + "\t\t -> ParentProcessID=" + me.th32ParentProcessID);

				// else
				// System.out.println("not added");

			}
			while (MyKernel32.INSTANCE.Process32Next(processes, me));
		}
		else
			System.out.println("get process list: cannot access first process in list ");

		MyKernel32.INSTANCE.CloseHandle(processes);

		return result;
	}

	/** The levels. */
	int					levels;

	/** The _pf counter. */
	private PdhCounter	_pfCounter;

	/** The _v mem counter. */
	private PdhCounter	_vMemCounter;

	/** The _cpu counter. */
	private PdhCounter	_cpuCounter;

	/** The _p mem counter. */
	private PdhCounter	_pMemCounter;

	private PdhCounter	_threadCounter;
	private PdhCounter	_handleCounter;

	private boolean		_started	= false;

	/**
	 * Gets the process tree.
	 * 
	 * @param pid
	 *            the pid
	 * 
	 * @return the process tree
	 */
	static public List getProcessTree(int pid)
	{
		Map[] maps = getProcessMaps(pid);
		Map processMap = maps[0];
		Map childrenMap = maps[1];
		Collection pids = new ArrayList();
		pids.add(new Integer(pid));
		return getProcessTree(childrenMap, pids);
	}

	/**
	 * Gets the process tree.
	 * 
	 * @param childrenMap
	 *            the children map
	 * @param pids
	 *            the pids
	 * 
	 * @return the process tree
	 */
	static List getProcessTree(Map childrenMap, Collection pids)
	{
		List result = new ArrayList();
		if (pids == null)
			return result;
		if (pids.isEmpty())
			return result;
		for (Iterator it = pids.iterator(); it.hasNext();)
		{
			Integer i = (Integer) it.next();
			// System.out.println(i);
			result.addAll(getProcessTree(childrenMap, (Collection) childrenMap.get(i)));
		}
		result.addAll(pids);
		return result;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.os.Process#killTree(int)
	 */
	public boolean killTree(int code)
	{
		if (_pid <= 0)
		{
			log("cannot kill process with pid " + _pid);
			return false;
		}

		if (!isRunning())
			return false;
		boolean result = true;
		List tree = getProcessTree(_pid);
		int retry = 0;
		while (tree.size() < 2 && retry < 20)
		{
			if (_debug)
				log("killTree: getProcessTree error: retrying ");
			tree = getProcessTree(_pid);
			retry++;
		}
		for (Iterator it = tree.iterator(); it.hasNext();)
		{
			int pid = ((Integer) it.next()).intValue();
			if (pid != _pid)
				result = result && kill(pid, code);

		}

		result = result && kill(code);

		return result;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.os.AbstractProcess#getExitCode()
	 */
	@Override
	public int getExitCode()
	{
		int result = 0;
		if (_exitCode < 0 && _processInformation != null)
		{
			result = getExitCodeInternal();
			if (result != MyKernel32.STILL_ACTIVE)
				setExitCode(result);
			else
				setExitCode(-2);
		}
		else
		{
			// log("getExitCode "+_exitCode + " "+_processInformation);
		}
		if (isDebug())
			log("getExitCode " + _exitCode + " processINFO==null=" + (_processInformation == null));
		// System.out.println("get exit code "+_exitCode);
		return _exitCode;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.os.Process#isRunning()
	 */
	public boolean isRunning()
	{
		if (_pid <= 0)
		{
			if (isDebug())
				log("is running: false pid=(" + _pid + "<=0)");
			// log("is running: false "+_pid);
			return false;
		}
		if (_processInformation == null)
		{
			if (isDebug())
				log("is running: _processInformation == null pid=" + _pid);
			return false;
		}
		// return _processInformation != null && getExitCode() < 0 && _pid > 0;
		boolean result = getExitCode() == -2 && _pid >= 0;
		// log("is running: "+result +" "+_pid + " "+ _exitCode);
		if (isDebug())
			log("is running: " + result + " " + _pid + " " + _exitCode);
		return result;
		/*
		 * Pointer process =
		 * MyKernel32.INSTANCE.OpenProcess(MyKernel32.PROCESS_QUERY_INFORMATION,
		 * false, _pid); if (process == Pointer.NULL) {
		 * log("is running: false "+_pid); return false; }
		 * MyKernel32.INSTANCE.CloseHandle(process);
		 * log("is running: true "+_pid); return true;
		 */

	}

	// if you use counters: you will have to destroy before finalze is called.
	// Otherwise the JVM may crash
	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.os.Process#destroy()
	 */
	volatile boolean	destroyed	= false;

	public void destroy()
	{
		if (destroyed)
			return;
		destroyed = true;
		if (_processInformation != null)
		{
			if (_teeName != null && _inputStream != null)
			{
				try
				{
					_inputStream.close();
				}
				catch (Exception e)
				{
					e.printStackTrace();
				}
				try
				{
					_outputStream.close();
				}
				catch (Exception e)
				{
					e.printStackTrace();
				}
				try
				{
					_errorStream.close();
				}
				catch (Exception e)
				{
					e.printStackTrace();
				}
				_inputStream = null;
				_outputStream = null;
				_errorStream = null;
				new File(_tmpPath, "in_" + _teeName).delete();
				new File(_tmpPath, "out_" + _teeName).delete();
				new File(_tmpPath, "err_" + _teeName).delete();

			}
			// else
			// System.out.println("no streams to destroy");

			if (outRead != null && outRead.getValue() != Pointer.NULL)
			{
				//MyKernel32.INSTANCE.SetHandleInformation(outRead.getValue(), 2, 0);
				//MyKernel32.INSTANCE.CloseHandle(outRead.getValue());
				outRead = null;
			}

			if (errRead != null && errRead.getValue() != Pointer.NULL)
			{
				//MyKernel32.INSTANCE.SetHandleInformation(errRead.getValue(), 2, 0);
				//MyKernel32.INSTANCE.CloseHandle(errRead.getValue());
				errRead = null;
			}

			if (inWrite != null && inWrite.getValue() != Pointer.NULL)
			{
				//MyKernel32.INSTANCE.SetHandleInformation(inWrite.getValue(), 2, 0);
				//MyKernel32.INSTANCE.CloseHandle(inWrite.getValue());
				inWrite = null;
			}

			if (_processInformation.hThread != null)
				if (!_processInformation.hThread.equals(Pointer.NULL))
					MyKernel32.INSTANCE.CloseHandle(_processInformation.hThread);
			if (_processInformation.hProcess != null)
				if (!_processInformation.hProcess.equals(Pointer.NULL))
					MyKernel32.INSTANCE.CloseHandle(_processInformation.hProcess);
			if (_cpuCounter != null)
			{
				_cpuCounter.close();
				_cpuCounter = null;
			}
			if (_vMemCounter != null)
			{
				_vMemCounter.close();
				_vMemCounter = null;
			}
			if (_pMemCounter != null)
			{
				_pMemCounter.close();
				_pMemCounter = null;
			}

			if (_pfCounter != null)
			{
				_pfCounter.close();
				_pfCounter = null;
			}
			if (_threadCounter != null)
			{
				_threadCounter.close();
				_threadCounter = null;
			}
			if (_handleCounter != null)
			{
				_handleCounter.close();
				_handleCounter = null;
			}
		}
		if (_debug)
			log("process handles destroyed " + _pid);
		//if (_processInformation != null)
		//_processInformation.finalize();
		_processInformation = null;

		//if (_startupInfo != null)
		//_startupInfo.finalize();
		_startupInfo = null;
		try
		{
			Thread.sleep(100);
		}
		catch (InterruptedException e)
		{
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		/*
		 * if (_pipeStreams) { if (inRead != null) if (inRead.getValue() !=
		 * null) MyKernel32.INSTANCE.CloseHandle(inRead.getValue()); if
		 * (outWrite != null) if (outWrite.getValue() != null)
		 * MyKernel32.INSTANCE.CloseHandle(outWrite.getValue()); if (errWrite !=
		 * null) if (errWrite.getValue() != null)
		 * MyKernel32.INSTANCE.CloseHandle(errWrite.getValue()); }
		 * 
		 * if (_outputStream != null) { try { _outputStream.close(); } catch
		 * (IOException e) { } _outputStream = null; }
		 * 
		 * if (_errorStream != null) { try { _errorStream.close(); } catch
		 * (IOException e) { } _errorStream = null; }
		 * 
		 * if (_inputStream != null) { try { _inputStream.close(); } catch
		 * (IOException e) { } _inputStream = null; }
		 */

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#finalize()
	 */
	@Override
	public void finalize() throws Throwable
	{
		try
		{
			// this may cause jvm crash when java shuts down -> TODO
			// destroy();
		}
		finally
		{
			super.finalize();
		}
	}

	/**
	 * Read virtual memory to structure.
	 * 
	 * @param baseAddress
	 *            the base address
	 * @param goal
	 *            the goal
	 * 
	 * @return true, if successful
	 */
	boolean readVirtualMemoryToStructure(Pointer baseAddress, Structure goal)
	{
		int size = goal.size();
		// System.out.println("readVirtualMemoryToStructure "+size);
		int ret = Ntdll.INSTANCE.ZwReadVirtualMemory(_processInformation.hProcess.getPointer(), baseAddress, goal.getPointer(), size, null);
		if (ret != 0)
			log("pid " + _pid + " ZwReadVirtualMemory returns " + Integer.toHexString(ret));

		goal.read();
		return ret == 0;

	}

	/**
	 * Read virtual memory to memory.
	 * 
	 * @param baseAddress
	 *            the base address
	 * @param goal
	 *            the goal
	 * 
	 * @return true, if successful
	 */
	boolean readVirtualMemoryToMemory(Pointer baseAddress, Memory goal)
	{
		int size = (int) goal.getSize();
		// System.out.println("readVirtualMemoryToMemory "+size);
		int ret = Ntdll.INSTANCE.ZwReadVirtualMemory(_processInformation.hProcess.getPointer(), baseAddress, goal, size, null);
		if (ret != 0)
		{
			if (ret == 0x8000000d) // see more http://nologs.com/ntstatus.html
				log("pid " + _pid + " ZwReadVirtualMemory returns " + Integer.toHexString(ret)+ " partial copy ");
			else
			  log("pid " + _pid + " ZwReadVirtualMemory returns " + Integer.toHexString(ret));
		}

		return ret == 0;

	}
	
	/**
	 * readProcessMemory to memory.
	 */
	long readProcessMemory(Pointer baseAddress, Memory goal)
	{
		NativeLong sizeAvalaible = new NativeLong(goal.size());
		NativeLongByReference bytesReadRefernce = new NativeLongByReference();
		boolean ret = MyKernel32.INSTANCE.ReadProcessMemory(_processInformation.hProcess.getPointer(), baseAddress, goal,
				sizeAvalaible, bytesReadRefernce);
		if (!ret)
			log("pid " + _pid + " ReadProcessMemory returns " + ret);
		long bytesRead = bytesReadRefernce.getValue().longValue();
		return bytesRead;

	}

	/**
	 * Gets the command line internal. this works only for 32 bit processes
	 * 
	 * @return the command line internal
	 */
	String getCommandLineInternal()
	{
		// System.out.println("get command internal "+getPid());
		String result = "?";
		PROCESS_BASIC_INFORMATION pbi = null;

		pbi = new PROCESS_BASIC_INFORMATION();
		IntByReference returnLength = new IntByReference();
		HANDLE hProcess = _processInformation.hProcess;
		int pbiSize = pbi.size(); // x64 = 48 bytes, x32 = 24
		int ret = Ntdll.INSTANCE.ZwQueryInformationProcess(hProcess, (byte) 0, pbi.getPointer(), pbiSize, returnLength);
		if (ret == 0)
		{
			pbi.read();
			if (pbi.PebBaseAddress != null)
			{
				PEB peb = new PEB();
				// System.out.println(""+1);
				if (readVirtualMemoryToStructure(pbi.PebBaseAddress, peb))
					if (peb.ProcessParameters != null)
					{
						RTL_USER_PROCESS_PARAMETERS userParams = new RTL_USER_PROCESS_PARAMETERS();
						int userParamsSize = userParams.size(); //x32 = 784, x64 = 1264
						// System.out.println(""+2);
						if (readVirtualMemoryToStructure(peb.ProcessParameters, userParams))
						{
							// System.out.println("MaximumLength "+userParams.CommandLine.MaximumLength);
							if (userParams.CommandLine.MaximumLength > 0)
							{
								Memory stringBuffer = new Memory(userParams.CommandLine.MaximumLength);
								// System.out.println(""+3);
								if (readVirtualMemoryToMemory(userParams.CommandLine.Buffer, stringBuffer))
									result = stringBuffer.getString(0, true);
							}					
							
							if (userParams.CurrentDirectoryPath.MaximumLength > 0)
							{
								Memory stringBuffer = new Memory(userParams.CurrentDirectoryPath.MaximumLength);
								if (readVirtualMemoryToMemory(userParams.CurrentDirectoryPath.Buffer, stringBuffer))
									_workingDir = stringBuffer.getString(0, true);
							}
							if (userParams.WindowTitle.MaximumLength > 0)
							{
								Memory stringBuffer = new Memory(userParams.WindowTitle.MaximumLength);
								if (readVirtualMemoryToMemory(userParams.WindowTitle.Buffer, stringBuffer))
									_title = stringBuffer.getString(0, true);
							}
							if (userParams.Environment != null)
							{
								// get size of environment strings
								MEMORY_BASIC_INFORMATION memInfo = new MEMORY_BASIC_INFORMATION();
								int memInfoSize = memInfo.size(); //x64 = 48, x32 = 28
								int bytesRead = MyKernel32.INSTANCE.VirtualQueryEx(hProcess.getPointer(), userParams.Environment, memInfo.getPointer(),
										memInfoSize);
								memInfo.read();
								if (bytesRead == 0)
								{
									_logger.warning("error getting environment in VirtualQueryEx " + Native.getLastError());
								}
								else if (MyKernel32.PAGE_NOACCESS == memInfo.Protect || MyKernel32.PAGE_EXECUTE == memInfo.Protect)
								{
									_logger.warning("error getting environment in VirtualQueryEx no access right");
								}
								else
								{
									long envSize = Math.min(Pointer.nativeValue(memInfo.RegionSize), 32767); //Max Size http://msdn.microsoft.com/en-us/library/ms682653%28v=vs.85%29.aspx				
									
									Memory mem = new Memory(envSize);
									readProcessMemory(userParams.Environment, mem);

									List<String> envStrings = new ArrayList<String>();
									String env = null;
									int l = 0;
									while (!"".equals(env))
									{
										env = mem.getString(l, true);
										if (env != null && env.length() != 0)
										{
											envStrings.add(env);
											l += env.length() * 2 + 2;
										}
										if (env == null)
											break;
									}

									parseEnvString(envStrings);
								}
							}
						}

					}
			}
		}
		// else
		// System.out.println("3 pid " + _pid +
		// " ZwQueryInformationProcess returns " + Integer.toHexString(ret));
		if (result != null)
			result = result.trim();
		return result;

	}

	private void parseEnvString(List<String> envStrings)
	{
		if (envStrings == null || envStrings.size() == 0)
			return;
		for (String str : envStrings)
		{
			String[] var = str.split("=");
			if (var.length == 2)
				_environment.add(new String[]
				{ var[0], var[1] });
		}

	}

	// this will work only if we run on 64.
	// if we run on wow64 (eg 32 bit), ZwQueryInformationProcess returns 0, but
	// PEB64.ProcessParameters is empty
	String getCommandLineInternal64()
	{
		log("get command internal 64 " + getPid());
		String result = "?";
		PROCESS_BASIC_INFORMATION pbi = null;

		pbi = new PROCESS_BASIC_INFORMATION();
		IntByReference returnLength = new IntByReference();
		HANDLE hProcess = _processInformation.hProcess;
		int size = pbi.size();
		int ret = Ntdll.INSTANCE.ZwQueryInformationProcess(hProcess, (byte) 0, pbi.getPointer(), size, returnLength);
		if (ret == 0)
		{
			pbi.read();
			if (pbi.PebBaseAddress != null)
			{
				PEB64 peb = new PEB64();
				// System.out.println("64 " + 1);
				if (readVirtualMemoryToStructure(pbi.PebBaseAddress, peb))
					if (peb.ProcessParameters != null)
					{
						RTL_USER_PROCESS_PARAMETERS userParams = new RTL_USER_PROCESS_PARAMETERS();
						// System.out.println("64 " + 2);
						if (readVirtualMemoryToStructure(peb.ProcessParameters, userParams))
						{
							// System.out.println("MaximumLength " +
							// userParams.CommandLine.MaximumLength);
							// System.out.println("Length " +
							// userParams.CommandLine.Length);
							Memory stringBuffer = new Memory(userParams.CommandLine.Length);
							// System.out.println("64 " + 3);
							if (readVirtualMemoryToMemory(userParams.CommandLine.Buffer, stringBuffer))
								result = stringBuffer.getString(0, true);
						}

					}
			}
		}
		else
			log("pid " + _pid + " ZwQueryInformationProcess returns " + Integer.toHexString(ret));
		return result;

	}

	// this should run on all platforms
	// TODO optimize by calling windows methods for WMI
	// note: Runtime.exec("cmd /C wmic") hangs
	// note: we cannot use p.getInputStream() since the result stream contains
	// unexpeced characters
	// note: when we write the result to file we have to convert the string.
	public String getCommandLineInternalWMI()
	{
		String result = "?";
		WindowsXPProcess p = null;
		// if the server is overloaded we may not get an answer -> try 3 times
		for (int k = 0; k < 3 && "?".equals(result); k++)
			try
			{

				p = new WindowsXPProcess();
				new File("wmic.tmp").delete();
				p.setCommand("cmd /C wmic process where processid=" + getPid() + " get commandline > wmic.tmp");
				p.setVisible(false);
				p.start();
				p.waitFor(30000);
				if (p.isRunning())
					p.kill(99);
				int ec = p.getExitCode();
				if (ec != 0)
					log("unexptected exit code in getCommandLineInternalWMI: "+ec);
				BufferedReader br = new BufferedReader(new FileReader("wmic.tmp"));
				String l = "?";
				try 
				{
				br.readLine();
				br.readLine();
				l = br.readLine();
				if (l.codePointAt(0) == 0)
				{
					StringBuffer s = new StringBuffer();
					for (int i = 0; i < l.length(); i++)
						if (l.codePointAt(i) != 0)
							s.append(l.charAt(i));
					l = s.toString();
				}
				result = l;
				}
				catch (Exception ex)
				{
					ex.printStackTrace();
				}
				br.close();
				p.destroy();
			}
			catch (Exception e)
			{
				if (_debug)
					log("Error in getCommandLineInternalWMI");
				e.printStackTrace();
				try
				{
					Thread.sleep(10000);
				}
				catch (InterruptedException e1)
				{
					e1.printStackTrace();
					return result;
				}
				if (p != null)
					p.destroy();
			}
		return result;

	}

	/**
	 * Gets the total cpu.
	 * 
	 * @return the total cpu
	 */
	public long getTotalCPU()
	{
		long result = -1;
		if (!isRunning())
			return -1;
		LongByReference lpCreationTime = new LongByReference();
		LongByReference lpExitTime = new LongByReference();
		LongByReference lpKernelTime = new LongByReference();
		LongByReference lpUserTime = new LongByReference();

		if (MyKernel32.INSTANCE.GetProcessTimes(_processInformation.hProcess, lpCreationTime, lpExitTime, lpKernelTime, lpUserTime))
			result = lpUserTime.getValue() + lpKernelTime.getValue();
		return result;
	}

	/**
	 * Current process id.
	 * 
	 * @return the int
	 */
	public static int currentProcessId()
	{
		return MyKernel32.INSTANCE.GetCurrentProcessId();
	}

	/**
	 * Process id of active window.
	 * 
	 * @return the int
	 */
	public static int processIdOfActiveWindow()
	{
		HWND w = MyUser32.INSTANCE.GetForegroundWindow();
		IntByReference result = new IntByReference();
		MyUser32.INSTANCE.GetWindowThreadProcessId(w, result);
		return result.getValue();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.os.Process#getCurrentCpu()
	 */
	public int getCurrentCpu()
	{
		if (!isRunning() || getCpuCounter() == null)
			return -1;
		PdhCounter c = getCpuCounter();
		return c.getIntValue();
	}

	/**
	 * Gets the cpu counter.
	 * 
	 * @return the cpu counter
	 */
	private PdhCounter getCpuCounter()
	{
		if (_cpuCounter == null)
			_cpuCounter = Pdh.getProcessEnglishCounter(_pid, "% Processor Time");
		return _cpuCounter;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.os.Process#getCurrentPhysicalMemory()
	 */
	public int getCurrentPhysicalMemory()
	{
		if (!isRunning())
			return -1;
		PdhCounter c = getPMemCounter();
		return c.getIntValue();
	}

	/**
	 * Gets the p mem counter.
	 * 
	 * @return the p mem counter
	 */
	private PdhCounter getPMemCounter()
	{
		if (_pMemCounter == null)
			_pMemCounter = Pdh.getProcessEnglishCounter(_pid, "Private Bytes");
		return _pMemCounter;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.os.Process#getCurrentVirtualMemory()
	 */
	public int getCurrentVirtualMemory()
	{
		if (!isRunning() || getVMemCounter() == null)
			return -1;
		PdhCounter c = getVMemCounter();
		return c.getIntValue();
	}

	/**
	 * Gets the v mem counter.
	 * 
	 * @return the v mem counter
	 */
	private PdhCounter getVMemCounter()
	{
		if (_vMemCounter == null)
			_vMemCounter = Pdh.getProcessEnglishCounter(_pid, "Virtual Bytes");
		return _vMemCounter;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.os.Process#getCurrentPageFaults()
	 */
	public int getCurrentPageFaults()
	{
		if (!isRunning())
			return -1;
		PdhCounter c = getPfCounter();
		return c.getIntValue();
	}

	/**
	 * Gets the pf counter.
	 * 
	 * @return the pf counter
	 */
	private PdhCounter getPfCounter()
	{
		if (_pfCounter == null)
			_pfCounter = Pdh.getProcessEnglishCounter(_pid, "Page Faults/sec");
		return _pfCounter;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.os.Process#getChildren()
	 */
	public Collection getChildren()
	{
		return getProcessTree(_pid);
	}

	// test
	/**
	 * The main method.
	 * 
	 * @param args
	 *            the arguments
	 */
	public static void main(String[] args)
	{
		/*
		 * WindowsXPProcess[] p = new WindowsXPProcess[1]; for (int i = 0; i <
		 * p.length; i++) { p[i] = new WindowsXPProcess(); //
		 * p[i].setPipeStreams(true, false);
		 * p[i].setCommand("ping 127.0.0.1 -t");// "c:/driwin/dripc.exe");// //
		 * "java -cp yajsw.jar // org.rzo.yajsw.HelloWorld > // t.log"); //
		 * p[i].setWorkingDir("c:/driwin"); p[i].setVisible(false);
		 * p[i].setPipeStreams(true, false); } boolean done = false; while
		 * (!done) { done = true; System.out.println("START");
		 * 
		 * for (int i = 0; i < p.length; i++) {
		 * 
		 * p[i].start(); //
		 * System.out.println(p[i].getCommandLineInternalWMI());
		 * 
		 * / String line = null; int k = 0; try { InputStreamReader isr = new
		 * InputStreamReader(p[i].getInputStream()); BufferedReader br = new
		 * BufferedReader(isr);
		 * 
		 * line = br.readLine(); System.out.println(line); while (k < 30 && line
		 * != null) { System.out.println(line); line = br.readLine(); k++; }
		 * 
		 * } catch (Exception e) { // TODO Auto-generated catch block
		 * e.printStackTrace(); } /
		 * 
		 * System.out.println("sleep"); p[i].waitFor(5000); }
		 * 
		 * System.out.println("KILL"); for (int i = 0; i < p.length; i++) { //
		 * p[i].killTree(999); ((WindowsXPProcess) p[i]).stop(5000, 999);
		 * System.out.println(p[i].getExitCode()); // p[i].finalize(); } try {
		 * Thread.sleep(1000); } catch (InterruptedException e) { // TODO
		 * Auto-generated catch block e.printStackTrace(); } }
		 * 
		 * // p.setCommand("java -classpath z:\dev\yajsw\wrapper.jar org.rzo." )
		 */
		/*
		 * WindowsXPProcess p = new WindowsXPProcess(); p.setCommand("notepad");
		 * p.setUser("test\\yajsw"); p.setPassword("yajsw"); p.start();
		 */
		// getProcess(3332);
		/*
		Process p = new WindowsXPProcess();
		// p.setCommand("ping 127.0.0.1");
		p.setCommand("set.bat");
		List<String[]> env = OperatingSystem.instance().processManagerInstance().getProcess(
				OperatingSystem.instance().processManagerInstance().currentProcessId()).getEnvironment();
		p.setEnvironment(env);
		System.out.println(p.getEnvironmentAsMap().get("Path"));
		System.out.println(env.get(0)[0]);
		p.setPipeStreams(true, false);
		p.start();
		String line = null;
		int k = 0;
		try
		{
			InputStreamReader isr = new InputStreamReader(p.getInputStream());
			BufferedReader br = new BufferedReader(isr);
			line = br.readLine();
			System.out.println(line);
			while (k < 30 && line != null)
			{
				System.out.println(line);
				line = br.readLine();
				k++;
			}
		}
		catch (Exception ex)
		{
			ex.printStackTrace();
		}
		*/

		/*
		System.out.println("start -----------------");
		WindowsXPProcess p2 = new WindowsXPProcess();
		elevating command line java   -jar "Z:\dev\yajsw\bat\/..\wrapper.jar" -t conf/wr
		apper.helloworld.conf
		p2.setCommand("java -jar wrapper.jar -t Z:\\dev\\yajsw\\conf\\wrapper.helloworld.conf" );
		p2.setDebug(true);
		if (p2.start())
		{
		System.out.println(p2.isElevated());
		p2.waitFor();
		System.out.println(p2.getPid());
		System.out.println(p2.getExitCode());
		}
		*/

		/*
		System.out.println("stop -----------------");
		WindowsXPProcess p = new WindowsXPProcess();
		p.setCommand("\"java\" -Xmx5m \"-Dtest=Ttest 1\" org.Test abc");
		p.setCommand("\"java\" -version");
		p.setCommand("java   -jar \"Z:\\dev\\yajsw\\bat\\/..\\wrapper.jar\" -t conf/wrapper.helloworld.conf" );
		p.setDebug(true);
		if (p.startElevated())
		{
		System.out.println(p.isElevated());
		p.waitFor();
		System.out.println(p.getPid());
		System.out.println(p.getExitCode());
		}
		*/
		WindowsXPProcess p = (WindowsXPProcess) getProcess(4664);
		for (int i=0; i<4; i++)
		{
			try
			{
				p.sendKey('D');
				Thread.sleep(10000);
			}
			catch (InterruptedException e)
			{
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		
		
		//p.sendKey('B');
		//p.sendKey('C');
		//p.sendKey('D');


	}

	/**
	 * Reconnect streams.
	 * 
	 * @return true, if successful
	 */
	public boolean reconnectStreams()
	{
		if (_teeName != null)
			try
			{
				_inputStream = new CyclicBufferFileInputStream(new File(_tmpPath, "out_" + _teeName));
				_errorStream = new CyclicBufferFileInputStream(new File(_tmpPath, "err_" + _teeName));
				_outputStream = new CyclicBufferFilePrintStream(new File(_tmpPath, "in_" + _teeName));
				return true;
			}
			catch (Exception ex)
			{
				ex.printStackTrace();
			}

		return false;

	}

	/**
	 * Writefd.
	 * 
	 * @param fd
	 *            the fd
	 * @param pointer
	 *            the pointer
	 */
	private void writefd(FileDescriptor fd, Pointer pointer)
	{
		try
		{
			// Field[] fields = FileDescriptor.class.getDeclaredFields();
			// System.out.println("fields");
			// for (Field field : fields){
			// System.out.println(field.getName());
			// }
			// System.out.println("writefd");
			Field handleField = FileDescriptor.class.getDeclaredField("handle");
			handleField.setAccessible(true);
			Field peerField = Pointer.class.getDeclaredField("peer");
			peerField.setAccessible(true);
			long value = peerField.getLong(pointer);
			// System.out.println(value);
			// System.out.flush();
			handleField.setLong(fd, value);
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}

	}

	public String currentUser()
	{
		String result = System.getenv("USERDOMAIN") + "\\" + System.getenv("USERNAME");
		result = result.toUpperCase();
		return result;
	}

	public String currentUserName()
	{
		String result = System.getProperty("user.name");
		if (result == null)
			return "";
		result = result.toUpperCase();
		return result;
	}

	public String currentUserDomain()
	{
		String result = System.getenv("USERDOMAIN");
		if (result == null)
			return "";
		return result.toUpperCase();
	}

	public String standardizeUser(String user)
	{
		if (user == null)
			return null;
		if (user.indexOf("\\") == -1)
			return currentUserDomain() + "\\" + user.toUpperCase();
		return user.toUpperCase();
	}

	boolean doesUserHavePrivilege(String lpPrivilegeName)

	{
		PointerByReference hToken = new PointerByReference();
		IntByReference dwSize = new IntByReference();
		Memory lpPrivileges;
		MyAdvapi.LUID PrivilegeLuid = new MyAdvapi.LUID();
		int i;
		boolean bResult = false;

		if (!MyAdvapi.INSTANCE.OpenProcessToken(MyKernel32.INSTANCE.GetCurrentProcess(), MyAdvapi.INSTANCE.TOKEN_QUERY, hToken))
			return false;

		MyAdvapi.INSTANCE.GetTokenInformation(hToken.getValue(), MyAdvapi.TokenPrivileges, null, 0, dwSize);

		lpPrivileges = new Memory(dwSize.getValue());

		if (!MyAdvapi.INSTANCE.GetTokenInformation(hToken.getValue(), MyAdvapi.TokenPrivileges, lpPrivileges, dwSize.getValue(), dwSize))
		{
			return false;
		}

		MyKernel32.INSTANCE.CloseHandle(hToken.getValue());

		if (!MyAdvapi.INSTANCE.LookupPrivilegeValueA(null, lpPrivilegeName, PrivilegeLuid))
		{
			return false;
		}

		MyAdvapi.TOKEN_PRIVILEGES privileges = new MyAdvapi.TOKEN_PRIVILEGES(lpPrivileges);
		for (i = 0; i < privileges.PrivilegeCount; i++)
		{
			if (privileges.Privileges[i].Luid.HighPart == PrivilegeLuid.HighPart && privileges.Privileges[i].Luid.LowPart == PrivilegeLuid.LowPart)
			{
				return true;
			}
		}
		return false;
	}

	public int getCurrentHandles()
	{
		if (!isRunning() || getHandlesCounter() == null)
			return -1;
		PdhCounter c = getHandlesCounter();
		return c.getIntValue();
	}

	private PdhCounter getHandlesCounter()
	{
		if (_handleCounter == null)
			_handleCounter = Pdh.getProcessEnglishCounter(_pid, "Handle Count");
		return _handleCounter;
	}

	public int getCurrentThreads()
	{
		if (!isRunning() || getThreadsCounter() == null)
			return -1;
		PdhCounter c = getThreadsCounter();
		return c.getIntValue();
	}

	private PdhCounter getThreadsCounter()
	{
		if (_threadCounter == null)
			_threadCounter = Pdh.getProcessEnglishCounter(_pid, "Thread Count");
		return _threadCounter;
	}

	public boolean isTerminated()
	{
		return (_started && !isRunning());
	}

	public boolean changeWorkingDir(String name)
	{
		File f = new File(name);
		String dir;
		if (!f.exists() || !f.isDirectory())
		{
			System.out.println("setWorkingDirectory failed. file not found " + name);
			return false;
		}
		else
			try
			{
				dir = f.getCanonicalPath();
			}
			catch (IOException e)
			{
				e.printStackTrace();
				return false;
			}
		boolean result = MyKernel32.INSTANCE.SetCurrentDirectoryA(dir);
		if (result)
			System.setProperty("user.dir", dir);
		return result;
	}
	
	public boolean startElevated()
	{
		
		String file = getCmdFile();
		if (file == null)
		{
			log("startElevated: Error: error in command");
			return false;
		}
		String parameters = getCmdParameters();
		if (_debug)
			log("elevated exec: " + file + " "+ parameters);

		SHELLEXECUTEINFO lpExecInfo = new SHELLEXECUTEINFO();
		lpExecInfo.fMask = Shell32.SEE_MASK_NOCLOSEPROCESS;
		lpExecInfo.hwnd = null;
		lpExecInfo.lpFile = file;
		lpExecInfo.lpVerb = Shell32.VERB_RUNAS;
		lpExecInfo.nShow = Shell32.SW_SHOWMAXIMIZED;
		lpExecInfo.lpParameters = parameters;
		lpExecInfo.cbSize = lpExecInfo.size();
		
		boolean result = Shell32.INSTANCE.ShellExecuteEx(lpExecInfo);
		if (!result)
		{
			int err = Native.getLastError();
			System.out.println("Error: "+err+" "+Kernel32Util.formatMessageFromLastErrorCode(err));
		}
		else
		{
			_pid = MyKernel32.INSTANCE.GetProcessId(lpExecInfo.hProcess);
			_processInformation = new PROCESS_INFORMATION();
			_processInformation.dwProcessId = _pid;
			_processInformation.hProcess = lpExecInfo.hProcess;

		}
		return result;
	}

	private String getCmdParameters()
	{
		String result = "";
		int i = 0;
		if (_arrCmd != null)
		{
			for (String cmd : _arrCmd)
			{
				if (i != 0)
				{
				if (cmd.startsWith("\""))
					result += cmd + " ";
				else
					result += '"' + cmd + "\" ";
				}
				i++;
			}
		}
		else
		{
			if (_cmd.startsWith("\""))
			{
				result = _cmd.substring(_cmd.indexOf("\" ", 1)+2);
			}
			else
			{
				result = _cmd.substring(_cmd.indexOf(" "));
			}			
		}
		if ("".equals(result))
			result = null;
		return result;
	}

	private String getCmdFile()
	{
		if (_arrCmd != null)
		{
			return _arrCmd[0];
		}
		if (_cmd != null)
		{
			if (_cmd.startsWith("\""))
			{
				return _cmd.substring(1, _cmd.indexOf("\" ", 1));
			}
			return _cmd.substring(0, _cmd.indexOf(" "));
		}
		return null;
	}
	
	public boolean isElevated()
	{
		if (_isElevated > -1)
			return _isElevated == 1;
			_isElevated = isElevatedInternal();
		return _isElevated == 1;
	}

	private int isElevatedInternal()
	{
		try
		{
		PointerByReference hToken = new PointerByReference();
		IntByReference dwSize = new IntByReference();
		Memory lpElevation;

		if (!MyAdvapi.INSTANCE.OpenProcessToken(MyKernel32.INSTANCE.GetCurrentProcess(), MyAdvapi.INSTANCE.TOKEN_QUERY, hToken))
			return -1;

		MyAdvapi.INSTANCE.GetTokenInformation(hToken.getValue(), MyAdvapi.TokenElevation, null, 0, dwSize);

		lpElevation = new Memory(dwSize.getValue());

		if (!MyAdvapi.INSTANCE.GetTokenInformation(hToken.getValue(), MyAdvapi.TokenElevation, lpElevation, dwSize.getValue(), dwSize))
		{
			return -1;
		}

		MyKernel32.INSTANCE.CloseHandle(hToken.getValue());

		MyAdvapi.TOKEN_ELEVATION elevation = new MyAdvapi.TOKEN_ELEVATION(lpElevation);
		return elevation.isElevated() ? 1 : 0;
		}
		catch (Throwable ex)
		{
			ex.printStackTrace();
		}
		return -1;
	}
	
	public static boolean elevateMe()
	{
		WindowsXPProcess me = (WindowsXPProcess) getProcess(currentProcessId());
		System.out.println("elevating command line " + me.getCommand());
		if (PlatformEx.isWinVista() && !me.isElevated())
		{
			WindowsXPProcess elevatedMe = new WindowsXPProcess();
			elevatedMe.setCommand(me.getCommand());
			if (me._arrCmd != null)
				elevatedMe.setCommand(me._arrCmd);
			elevatedMe.setDebug(me._debug);
			elevatedMe.setLogger(me._logger);
			me.destroy();
			boolean result = elevatedMe.startElevated();
			if (result)
			{
				elevatedMe.waitFor();
				elevatedMe.destroy();
				return true;
			}
		}
		return false;
	}
	
	HWND lastActiveWindow = null;
	
	private boolean isActiveWindow(HWND wnd)
	{
        WINDOWINFO pwi = new WINDOWINFO();
        pwi.size();
        if (MyUser32.INSTANCE.GetWindowInfo(wnd, pwi))
        {
        	pwi.read();
        	return pwi.dwWindowStatus == 1;
        }
        return false;
		
	}
	
	public HWND getActiveWindow()
	{
		if (lastActiveWindow != null && isActiveWindow(lastActiveWindow))
				return lastActiveWindow;
		else
		{
			MyUser32.WNDENUMPROC findActiveWindow = new MyUser32.WNDENUMPROC()
			{
				
				// lParam is the pid of our process
				public boolean callback(HWND wnd, int lParam)
				{
					
					// remember first window
					//System.out.println("callback ");
					// get the pid of the window
					IntByReference dwID = new IntByReference();
					MyUser32.INSTANCE.GetWindowThreadProcessId(wnd, dwID);
					// if this windows belongs to our process
					if (dwID.getValue() == lParam)
					{
						// if we have no window, try the first one with a name.
						if (lastActiveWindow == null)
						{
							byte[] windowText = new byte[512];
							MyUser32.INSTANCE.GetWindowTextA(wnd, windowText, 512);
					        String wText = Native.toString(windowText);
					        System.out.println(wText);
					        if (!wText.isEmpty())
					        	lastActiveWindow = wnd;
						}
						if (isActiveWindow(wnd))
							lastActiveWindow = wnd;
					}
					// continue with next window
					return true;
				}
			};
			MyUser32.INSTANCE.EnumWindows(findActiveWindow, _pid);

		}
		byte[] windowText = new byte[512];


		return lastActiveWindow;
			
		
	}
	
	public void sendKey(final char key)
	{
		if (_pid <= 0)
			return;
		if (getActiveWindow() != null)
		{
			byte[] windowText = new byte[512];
			MyUser32.INSTANCE.GetWindowTextA(lastActiveWindow, windowText, 512);
	        String wText = Native.toString(windowText);
	        wText = (wText.isEmpty()) ? "" : "; text: " + wText;
	        System.out.println("sending key "+key+" to "+wText);

		MyUser32.INSTANCE.SendMessageW(lastActiveWindow, MyUser32.WM_KEYDOWN,  key, 0);
		MyUser32.INSTANCE.SendMessageW(lastActiveWindow, MyUser32.WM_KEYUP,  key, 0);
		}
		else
			System.out.println("no window found");

	}

}
