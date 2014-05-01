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

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.apache.commons.collections.MultiHashMap;
import org.rzo.yajsw.os.Keyboard;
import org.rzo.yajsw.os.ms.win.w32.DummyWindow.HotKey;
import org.rzo.yajsw.os.ms.win.w32.DummyWindow.WndListner;

import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.User32;

// TODO: Auto-generated Javadoc
/**
 * The Class WindowsXPKeyboard.
 */
public class WindowsXPKeyboard implements Keyboard
{

	/** The _instance. */
	static Keyboard	_instance;

	/**
	 * Instance.
	 * 
	 * @return the keyboard
	 */
	static synchronized public Keyboard instance()
	{
		if (_instance == null)
			_instance = new WindowsXPKeyboard();
		return _instance;
	}

	/**
	 * The Interface MyUser32.
	 */
	public interface MyUser32 extends User32
	{

		/** The INSTANCE. */
		MyUser32	INSTANCE	= (MyUser32) Native.loadLibrary("User32", MyUser32.class);

		/*
		 * LRESULT CALLBACK KeyboardProc( int code, WPARAM wParam, LPARAM lParam
		 * );
		 */
		/**
		 * The Interface KeyboardProc.
		 */
		interface KeyboardProc extends StdCallCallback
		{

			/**
			 * Callback.
			 * 
			 * @param code
			 *            the code
			 * @param wParam
			 *            the w param
			 * @param lParam
			 *            the l param
			 * 
			 * @return the int
			 */
			int callback(int code, int wParam, int lParam);
		}

		/*
		 * LRESULT CallNextHookEx( HHOOK hhk, int nCode, WPARAM wParam, LPARAM
		 * lParam );
		 */
		/**
		 * Call next hook ex.
		 * 
		 * @param hhk
		 *            the hhk
		 * @param nCode
		 *            the n code
		 * @param wParam
		 *            the w param
		 * @param lParam
		 *            the l param
		 * 
		 * @return the int
		 */
		int CallNextHookEx(Pointer hhk, int nCode, int wParam, int lParam);

		/*
		 * HHOOK SetWindowsHookEx( int idHook, HOOKPROC lpfn, HINSTANCE hMod,
		 * DWORD dwThreadId );
		 */
		/**
		 * Sets the windows hook ex a.
		 * 
		 * @param idHook
		 *            the id hook
		 * @param lpfn
		 *            the lpfn
		 * @param hMod
		 *            the h mod
		 * @param dwThreadId
		 *            the dw thread id
		 * 
		 * @return the pointer
		 */
		Pointer SetWindowsHookExA(int idHook, KeyboardProc lpfn, Pointer hMod, int dwThreadId);

		/** The W h_ keyboard. */
		int	WH_KEYBOARD	= 2;

		/*
		 * BOOL UnhookWindowsHookEx( HHOOK hhk );
		 */
		/**
		 * Unhook windows hook ex.
		 * 
		 * @param hhk
		 *            the hhk
		 * 
		 * @return true, if successful
		 */
		boolean UnhookWindowsHookEx(Pointer hhk);

		/*
		 * BOOL PostMessage( HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam
		 * );
		 */
		/**
		 * Post message a.
		 * 
		 * @param hWnd
		 *            the h wnd
		 * @param uMsg
		 *            the u msg
		 * @param wParam
		 *            the w param
		 * @param lParam
		 *            the l param
		 * 
		 * @return the int
		 */
		int PostMessageA(Pointer hWnd, int uMsg, int wParam, int lParam);

	}

	/*
	 * class KeyboardListnerClass implements KeyboardProc { KeyboardListner
	 * _listner; KeyboardListnerClass(KeyboardListner listner) { _listner =
	 * listner; }
	 * 
	 * public int callback(int code, int wParam, int lParam) {
	 * //_listner.keyRead(null); System.out.println("keyboard callback code: "
	 * +code + " wParam "+ wParam + " lParam "+lParam ); return
	 * MyUser32.INSTANCE.CallNextHookEx(null, code, wParam, lParam); } }
	 * 
	 * public boolean addListner(KeyboardListner listner) { if
	 * (_listners.get(listner) != null) return false; Pointer hinst = null;
	 * PointerByReference hhinst = new PointerByReference(); //if
	 * (MyKernel32.INSTANCE.GetModuleHandleExA(0, null, hhinst)) // hinst =
	 * hhinst.getPointer(); / if (hinst == null) hinst =
	 * MyKernel32.INSTANCE.GetModuleHandleA(null); Pointer handle = null;
	 * KeyboardListnerClass iListner = new KeyboardListnerClass(listner); if
	 * (hinst != null) handle =
	 * MyUser32.INSTANCE.SetWindowsHookExA(MyUser32.WH_KEYBOARD, iListner,
	 * hinst, 0); if (handle != null) { _listners.put(listner, handle); return
	 * true; } else { int er = MyKernel32.INSTANCE.GetLastError();
	 * System.out.println("error "+ Integer.toHexString(er)); }
	 * 
	 * return false; }
	 * 
	 * public boolean removeListner(KeyboardListner listner) { Pointer handle =
	 * (Pointer) _listners.get(listner); if (handle != null) { if
	 * (MyUser32.INSTANCE.UnhookWindowsHookEx(handle))
	 * _listners.remove(listner); return true; } return false; }
	 */

	/** The Constant WND_REGISTER_HOTKEY. */
	public static final int	WND_REGISTER_HOTKEY		= 999;

	/** The Constant WND_UNREGISTER_HOTKEY. */
	public static final int	WND_UNREGISTER_HOTKEY	= 998;

	/** The _dummy window. */
	DummyWindow				_dummyWindow			= DummyWindow.instance();

	/** The _keys. */
	MultiHashMap			_keys					= new MultiHashMap();

	/** The _listners. */
	Map						_listners				= new HashMap();

	/**
	 * Instantiates a new windows xp keyboard.
	 */
	public WindowsXPKeyboard()
	{
		try
		{
			_dummyWindow.waitTermination();
		}
		catch (InterruptedException e)
		{
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		WndListner hotKeyHandler = new WndListner()
		{

			public int execute(int uMsg, int wParam, int lParam)
			{
				HotKey k = (HotKey) DummyWindow._hotKeys.get(new Integer(wParam));
				if (k == null)
					return 0;
				Collection listners = _keys.getCollection(k);
				if (listners == null)
					return 0;
				for (Iterator it = listners.iterator(); it.hasNext();)
				{
					HotKeyListner listner = (HotKeyListner) it.next();
					try
					{
						listner.keyPressed();
					}
					catch (Exception ex)
					{
						ex.printStackTrace();
					}
				}
				return 0;
			}

		};
		_dummyWindow.addListner(new Integer(DummyWindow.MyUser32.WM_HOTKEY), hotKeyHandler);

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @seeorg.rzo.yajsw.os.Keyboard#registerHotkey(org.rzo.yajsw.os.Keyboard.
	 * HotKeyListner, int, int)
	 */
	public synchronized void registerHotkey(HotKeyListner listner, int mod, int key)
	{
		MyUser32.INSTANCE.PostMessageA(_dummyWindow._hWnd, WND_REGISTER_HOTKEY, mod, key);
		try
		{
			_dummyWindow.waitTermination();
		}
		catch (InterruptedException e)
		{
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		HotKey k = _dummyWindow.new HotKey(mod, key);
		_listners.put(listner, k);
		_keys.put(k, listner);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.rzo.yajsw.os.Keyboard#unregisterHotKey(org.rzo.yajsw.os.Keyboard.
	 * HotKeyListner)
	 */
	public synchronized void unregisterHotKey(HotKeyListner listner)
	{
		HotKey k = (HotKey) _listners.get(listner);
		if (k == null)
			return;
		_listners.remove(listner);
		_keys.remove(k, listner);
		Collection listners = _keys.getCollection(k);

		if (listners == null || listners.isEmpty())
		{
			MyUser32.INSTANCE.PostMessageA(_dummyWindow._hWnd, WND_UNREGISTER_HOTKEY, k._wParam, k._lParam);
			try
			{
				_dummyWindow.waitTermination();
			}
			catch (InterruptedException e)
			{
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

}
