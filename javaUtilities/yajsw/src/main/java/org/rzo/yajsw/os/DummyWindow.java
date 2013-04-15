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
package org.rzo.yajsw.os;

import java.util.Map;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.Semaphore;

import jnacontrib.jna.Options;

import org.apache.commons.collections.BidiMap;
import org.apache.commons.collections.MultiHashMap;
import org.apache.commons.collections.bidimap.DualHashBidiMap;
import org.rzo.yajsw.os.ms.win.w32.DummyWindow.MyUser32.WNDCLASSEX;
import org.rzo.yajsw.os.ms.win.w32.DummyWindow.MyUser32.WNDPROC;

import com.sun.jna.Callback;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import com.sun.jna.platform.win32.GDI32;
import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.User32;
import com.sun.jna.ptr.PointerByReference;

// TODO: Auto-generated Javadoc
/**
 * The Class DummyWindow.
 */
public class DummyWindow
{

	/**
	 * The Interface MyKernel32.
	 */
	public interface MyKernel32 extends Kernel32
	{

		/** The INSTANCE. */
		MyKernel32	INSTANCE	= (MyKernel32) Native.loadLibrary("Kernel32", MyKernel32.class);

		/*
		 * HMODULE WINAPI GetModuleHandle( __in LPCTSTR lpModuleName );
		 */
		/**
		 * Gets the module handle a.
		 * 
		 * @param lpModuleName
		 *            the lp module name
		 * 
		 * @return the pointer
		 */
		Pointer GetModuleHandleA(String lpModuleName);

		/*
		 * BOOL WINAPI GetModuleHandleEx( __in DWORD dwFlags, __in LPCTSTR
		 * lpModuleName, __out HMODULE phModule );
		 */
		/**
		 * Gets the module handle ex a.
		 * 
		 * @param dwFlags
		 *            the dw flags
		 * @param lpModuleName
		 *            the lp module name
		 * @param phModule
		 *            the ph module
		 * 
		 * @return true, if successful
		 */
		boolean GetModuleHandleExA(int dwFlags, String lpModuleName, PointerByReference phModule);

		/**
		 * Global add atom a.
		 * 
		 * @param key
		 *            the key
		 * 
		 * @return the int
		 */
		int GlobalAddAtomA(String key);

	}// Kernel32

	/**
	 * The Interface MyUser32.
	 */
	public interface MyUser32 extends User32
	{
		// Method declarations, constant and structure definitions go here

		/** The INSTANCE. */
		MyUser32	INSTANCE	= (MyUser32) Native.loadLibrary("User32", MyUser32.class);

		/**
		 * Register hot key.
		 * 
		 * @param hWnd
		 *            the h wnd
		 * @param id
		 *            the id
		 * @param fsModifiers
		 *            the fs modifiers
		 * @param vk
		 *            the vk
		 * 
		 * @return the int
		 */
		int RegisterHotKey(Pointer hWnd, int id, int fsModifiers, int vk);

		/*
		 * BOOL UnregisterHotKey( HWND hWnd, int id );
		 */
		/**
		 * Unregister hot key.
		 * 
		 * @param hWnd
		 *            the h wnd
		 * @param id
		 *            the id
		 * 
		 * @return the int
		 */
		boolean UnregisterHotKey(Pointer hWnd, int id);

		/*
		 * LRESULT CALLBACK WindowProc( HWND hwnd, UINT uMsg, WPARAM wParam,
		 * LPARAM lParam );
		 */
		/**
		 * The Interface WNDPROC.
		 */
		interface WNDPROC extends Callback
		{

			/**
			 * Callback.
			 * 
			 * @param hwnd
			 *            the hwnd
			 * @param uMsg
			 *            the u msg
			 * @param wParam
			 *            the w param
			 * @param lParam
			 *            the l param
			 * 
			 * @return the int
			 */
			int callback(Pointer hwnd, int uMsg, int wParam, int lParam);
		}

		/** The W m_ hotkey. */
		int	WM_HOTKEY	= 786;

		/*
		 * typedef struct { UINT cbSize; UINT style; WNDPROC lpfnWndProc; int
		 * cbClsExtra; int cbWndExtra; HINSTANCE hInstance; HICON hIcon; HCURSOR
		 * hCursor; HBRUSH hbrBackground; LPCTSTR lpszMenuName; LPCTSTR
		 * lpszClassName; HICON hIconSm; } WNDCLASSEX,PWNDCLASSEX;
		 */
		/** The C s_ hredraw. */
		int	CS_HREDRAW	= 2;

		/** The C s_ vredraw. */
		int	CS_VREDRAW	= 1;

		/**
		 * The Class WNDCLASSEX.
		 */
		static class WNDCLASSEX extends Structure
		{

			/** The cb size. */
			public int		cbSize			= size();

			/** The style. */
			public int		style			= 0;

			/** The lpfn wnd proc. */
			public WNDPROC	lpfnWndProc;

			/** The cb cls extra. */
			public int		cbClsExtra		= 0;

			/** The cb wnd extra. */
			public int		cbWndExtra		= 0;

			/** The h instance. */
			public Pointer	hInstance;

			/** The h icon. */
			public Pointer	hIcon			= null;

			/** The h cursor. */
			public Pointer	hCursor			= null;

			/** The hbr background. */
			public Pointer	hbrBackground	= null;

			/** The lpsz menu name. */
			public String	lpszMenuName	= null;

			/** The lpsz class name. */
			public String	lpszClassName	= "JavaDummyWnd";

			/** The h icon sm. */
			public Pointer	hIconSm			= null;
		}

		/*
		 * ATOM RegisterClassEx( CONST WNDCLASSEXlpwcx );
		 */
		/**
		 * Register class ex a.
		 * 
		 * @param lpwcx
		 *            the lpwcx
		 * 
		 * @return the int
		 */
		int RegisterClassExA(WNDCLASSEX lpwcx);

		/*
		 * BOOL UnregisterClass( LPCTSTR lpClassName, HINSTANCE hInstance );
		 */
		/**
		 * Unregister class w.
		 * 
		 * @param lpClassName
		 *            the lp class name
		 * @param hInstance
		 *            the h instance
		 * 
		 * @return the int
		 */
		int UnregisterClassW(String lpClassName, Pointer hInstance);

		/*
		 * HWND CreateWindowEx( DWORD dwExStyle, LPCTSTR lpClassName, LPCTSTR
		 * lpWindowName, DWORD dwStyle, int x, int y, int nWidth, int nHeight,
		 * HWND hWndParent, HMENU hMenu, HINSTANCE hInstance, LPVOID lpParam );
		 */

		/** The W s_ overlappedwindow. */
		int	WS_OVERLAPPEDWINDOW	= 0xcf0000;

		/** The W s_ e x_ clientedge. */
		int	WS_EX_CLIENTEDGE	= 512;

		/** The W s_ overlapped. */
		int	WS_OVERLAPPED		= 0;

		/** The W s_ visible. */
		int	WS_VISIBLE			= 0x10000000;

		/**
		 * Creates the window ex a.
		 * 
		 * @param dwExStyle
		 *            the dw ex style
		 * @param lpClassName
		 *            the lp class name
		 * @param lpWindowName
		 *            the lp window name
		 * @param dwStyle
		 *            the dw style
		 * @param x
		 *            the x
		 * @param y
		 *            the y
		 * @param nWidth
		 *            the n width
		 * @param nHeight
		 *            the n height
		 * @param hWndParent
		 *            the h wnd parent
		 * @param hMenu
		 *            the h menu
		 * @param hInstance
		 *            the h instance
		 * @param lpParam
		 *            the lp param
		 * 
		 * @return the pointer
		 */
		Pointer CreateWindowExA(int dwExStyle, String lpClassName, String lpWindowName, int dwStyle, int x, int y, int nWidth, int nHeight,
				Pointer hWndParent, Pointer hMenu, Pointer hInstance, Pointer lpParam);

		/*
		 * LONG_PTR SetWindowLongPtr( HWND hWnd, int nIndex, LONG_PTR dwNewLong
		 * );
		 */
		/**
		 * Sets the window long a.
		 * 
		 * @param hWnd
		 *            the h wnd
		 * @param nIndex
		 *            the n index
		 * @param dwNewLong
		 *            the dw new long
		 * 
		 * @return the pointer
		 */
		Pointer SetWindowLongA(Pointer hWnd, int nIndex, Pointer dwNewLong);

		/*
		 * LONG_PTR GetWindowLongPtr( HWND hWnd, int nIndex );
		 */
		/**
		 * Gets the window long a.
		 * 
		 * @param hWnd
		 *            the h wnd
		 * @param nIndex
		 *            the n index
		 * 
		 * @return the pointer
		 */
		Pointer GetWindowLongA(Pointer hWnd, int nIndex);

		/** The GWL p_ userdata. */
		int	GWLP_USERDATA	= -21;

		/*
		 * LRESULT DefWindowProc( HWND hWnd, UINT Msg, WPARAM wParam, LPARAM
		 * lParam );
		 */
		/**
		 * Def window proc a.
		 * 
		 * @param hWnd
		 *            the h wnd
		 * @param Msg
		 *            the msg
		 * @param wParam
		 *            the w param
		 * @param lParam
		 *            the l param
		 * 
		 * @return the int
		 */
		int DefWindowProcA(Pointer hWnd, int Msg, int wParam, int lParam);

		/*
		 * BOOL ShowWindow( HWND hWnd, int nCmdShow );
		 */
		/**
		 * Show window.
		 * 
		 * @param hWnd
		 *            the h wnd
		 * @param nCmdShow
		 *            the n cmd show
		 * 
		 * @return the int
		 */
		int ShowWindow(Pointer hWnd, int nCmdShow);

		/** The S w_ hide. */
		int	SW_HIDE			= 0;

		/** The S w_ show. */
		int	SW_SHOW			= 5;

		/** The S w_ shownormal. */
		int	SW_SHOWNORMAL	= 1;

		/*
		 * BOOL UpdateWindow( HWND hWnd // handle to window );
		 */
		/**
		 * Update window.
		 * 
		 * @param hWnd
		 *            the h wnd
		 * 
		 * @return the int
		 */
		int UpdateWindow(Pointer hWnd // handle to window
		);

		/*
		 * typedef struct { HWND hwnd; UINT message; WPARAM wParam; LPARAM
		 * lParam; DWORD time; POINT pt; } MSG,PMSG;
		 */
		/**
		 * The Class MSG.
		 */
		class MSG extends Structure
		{

			/** The hwnd. */
			public Pointer	hwnd;

			/** The message. */
			public int		message;

			/** The w param. */
			public int		wParam;

			/** The l param. */
			public int		lParam;

			/** The time. */
			public int		time;

			/** The pt. */
			public POINT	pt;
		}

		/*
		 * BOOL GetMessage( LPMSG lpMsg, HWND hWnd, UINT wMsgFilterMin, UINT
		 * wMsgFilterMax );
		 */
		/**
		 * Gets the message a.
		 * 
		 * @param lpMsg
		 *            the lp msg
		 * @param hWnd
		 *            the h wnd
		 * @param wMsgFilterMin
		 *            the w msg filter min
		 * @param wMsgFilterMax
		 *            the w msg filter max
		 * 
		 * @return the int
		 */
		int GetMessageA(MSG lpMsg, Pointer hWnd, int wMsgFilterMin, int wMsgFilterMax);

		/*
		 * LRESULT DispatchMessage( const MSGlpmsg );
		 */
		/**
		 * Dispatch message a.
		 * 
		 * @param lpmsg
		 *            the lpmsg
		 * 
		 * @return the int
		 */
		int DispatchMessageA(MSG lpmsg);

	} // user32

	/**
	 * The Interface MyGdi32.
	 */
	public interface MyGdi32 extends GDI32
	{

		/** The INSTANCE. */
		MyGdi32	INSTANCE	= (MyGdi32) Native.loadLibrary("gdi32", MyGdi32.class, Options.UNICODE_OPTIONS);

		/*
		 * HGDIOBJ GetStockObject( int fnObject // stock object type );
		 */
		/**
		 * Gets the stock object.
		 * 
		 * @param fnObject
		 *            the fn object
		 * 
		 * @return the pointer
		 */
		Pointer GetStockObject(int fnObject);

		/** The BLAC k_ brush. */
		int	BLACK_BRUSH	= 4;

	}// Gdi32

	/**
	 * The Class CallbackMessage.
	 */
	static class CallbackMessage
	{

		/** The _u msg. */
		int	_uMsg;

		/** The _w param. */
		int	_wParam;

		/** The _l param. */
		int	_lParam;

		/**
		 * Instantiates a new callback message.
		 * 
		 * @param uMsg
		 *            the u msg
		 * @param wParam
		 *            the w param
		 * @param lParam
		 *            the l param
		 */
		CallbackMessage(int uMsg, int wParam, int lParam)
		{
			_uMsg = uMsg;
			_wParam = wParam;
			_lParam = lParam;
		}
	}

	/** The _instance. */
	static DummyWindow			_instance;

	/** The _listners. */
	static Map					_listners	= new MultiHashMap();

	/** The _wnd proc. */
	WndProc						_wndProc	= new WndProc();

	/** The _hinstance. */
	Pointer						_hinstance;

	/** The _h wnd. */
	Pointer						_hWnd;

	/** The _wnd class. */
	WNDCLASSEX					_wndClass	= new WNDCLASSEX();

	/** The _queue. */
	static LinkedBlockingQueue	_queue		= new LinkedBlockingQueue();

	/** The _hot keys. */
	static BidiMap				_hotKeys	= new DualHashBidiMap();

	/** The _semaphore. */
	Semaphore					_semaphore	= new Semaphore(0);

	/**
	 * The Class HotKey.
	 */
	public class HotKey
	{

		/**
		 * Instantiates a new hot key.
		 * 
		 * @param wParam
		 *            the w param
		 * @param lParam
		 *            the l param
		 */
		public HotKey(int wParam, int lParam)
		{
			_wParam = wParam;
			_lParam = lParam;
		}

		/** The _w param. */
		int	_wParam;

		/** The _l param. */
		int	_lParam;

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.lang.Object#hashCode()
		 */
		@Override
		public int hashCode()
		{
			return _wParam | _lParam;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.lang.Object#equals(java.lang.Object)
		 */
		@Override
		public boolean equals(Object obj)
		{
			return (obj instanceof HotKey && ((HotKey) obj)._lParam == _lParam && ((HotKey) obj)._lParam == _lParam);
		}
	}

	/**
	 * Instance.
	 * 
	 * @return the dummy window
	 */

	/**
	 * The Class WndProc.
	 */
	static class WndProc extends Structure implements WNDPROC
	{

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * org.rzo.yajsw.os.ms.win.xp.DummyWindow.MyUser32.WNDPROC#callback(
		 * com.sun.jna.Pointer, int, int, int)
		 */
		public int callback(Pointer hWnd, int uMsg, int wParam, int lParam)
		{
			// System.out.println("callback " + uMsg + " " + wParam + " " +
			// lParam + " " + "ptr" + " "+ hWnd);
			if (_listners.get(new Integer(uMsg)) != null)
			{
				CallbackMessage msg = new CallbackMessage(uMsg, wParam, lParam);
				_queue.offer(msg);
			}
			int res = MyUser32.INSTANCE.DefWindowProcA(hWnd, uMsg, wParam, lParam);
			// System.out.println(">" + res);
			return res;
		}

		/**
		 * Instantiates a new wnd proc.
		 */
		WndProc()
		{
			super();
			allocateMemory(4);
		}
	}

	/**
	 * The Interface WndListner.
	 */
	public interface WndListner
	{

		/**
		 * Execute.
		 * 
		 * @param uMsg
		 *            the u msg
		 * @param wParam
		 *            the w param
		 * @param lParam
		 *            the l param
		 * 
		 * @return the int
		 */
		int execute(int uMsg, int wParam, int lParam);
	}

	/**
	 * Adds the listner.
	 * 
	 * @param uMsg
	 *            the u msg
	 * @param listner
	 *            the listner
	 */
	public void addListner(Integer uMsg, WndListner listner)
	{
		synchronized (_listners)
		{
			_listners.put(uMsg, listner);
		}
	}

	/**
	 * Removes the listner.
	 * 
	 * @param listner
	 *            the listner
	 */
	public void removeListner(WndListner listner)
	{
		synchronized (_listners)
		{
			_listners.remove(listner);
		}

	}

	/**
	 * Inits the.
	 */

	/**
	 * Wait termination.
	 * 
	 * @throws InterruptedException
	 *             the interrupted exception
	 */
	void waitTermination() throws InterruptedException
	{
		// System.out.println("+ wait termination "+
		// System.currentTimeMillis());
		_semaphore.acquire();
		// System.out.println("- wait termination "+
		// System.currentTimeMillis());
	}
}
