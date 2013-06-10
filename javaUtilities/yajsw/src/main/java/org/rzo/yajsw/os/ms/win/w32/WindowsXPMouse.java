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

import java.util.concurrent.Executor;

import org.rzo.yajsw.os.Mouse;

import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.Structure;
import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.User32;
import com.sun.jna.platform.win32.BaseTSD.ULONG_PTR;
import com.sun.jna.platform.win32.WinDef.HWND;
import com.sun.jna.platform.win32.WinDef.LRESULT;
import com.sun.jna.platform.win32.WinDef.WPARAM;
import com.sun.jna.platform.win32.WinUser.HHOOK;
import com.sun.jna.platform.win32.WinUser.HOOKPROC;
import com.sun.jna.platform.win32.WinUser.MSG;
import com.sun.jna.platform.win32.WinUser.POINT;

// TODO: Auto-generated Javadoc
/**
 * The Class WindowsXPKeyboard.
 */
public class WindowsXPMouse implements Mouse
{
	
	
	public final User32 USER32INST;
	public final Kernel32 KERNEL32INST;
	public WindowsXPMouse()
	{
	    if(!Platform.isWindows())
	    {
	        throw new UnsupportedOperationException("Not supported on this platform.");
	    }
	    USER32INST = User32.INSTANCE;
	    KERNEL32INST = Kernel32.INSTANCE;
	    mouseHook=hookTheMouse();
	    Native.setProtected(true);

	}
	public static LowLevelMouseProc mouseHook;
	public static Runnable action;
	public static Mouse instance;
	public HHOOK hhk;
	public Thread thrd;
	public boolean threadFinish = true;
	public boolean isHooked = false;
	public static final int WM_MOUSEMOVE = 512;
	public static final int WM_LBUTTONDOWN = 513;
	public static final int WM_LBUTTONUP = 514;
	public static final int WM_RBUTTONDOWN = 516;
	public static final int WM_RBUTTONUP = 517;
	public static final int WM_MBUTTONDOWN = 519;
	public static final int WM_MBUTTONUP = 520;
	
	public static Mouse instance()
	{
		if (instance == null)
			instance = new WindowsXPMouse();
		return instance;
	}


	public synchronized void unregisterMouseUpListner()
	{
		if (thrd == null)
			return;
		//System.out.println("unregister ");
	    threadFinish = true;
	    if (thrd.isAlive())
	    {
	        thrd.interrupt();
	        thrd = null;
	    }
	    isHooked = false;
	}
	public boolean isIsHooked()
	{
	    return isHooked;
	}
	public synchronized void registerMouseUpListner(Runnable action, Executor executor)
	{
		if (thrd != null)
			return;
		this.action = action;
	    thrd = new Thread(new Runnable() {
	        
	        public void run()
	            {
	                  try
	                  {
	                        if(!isHooked)
	                        {   
	                            hhk = USER32INST.SetWindowsHookEx(14, (HOOKPROC) mouseHook,KERNEL32INST.GetModuleHandle(null),0);
	                            isHooked = true;
	                            MSG msg = new MSG();
	                            while ((USER32INST.GetMessage(msg, null, 0, 0)) != 0)
	                            {
	                            	System.out.println("got message");
	                                USER32INST.TranslateMessage(msg);     
	                                USER32INST.DispatchMessage(msg);
	                                System.out.print(isHooked);
	                                if (!isHooked)
	                                      break;
	                            }
	                        }
	                        else
	                            System.out.println("The Hook is already installed.");
	                }
	                catch (Exception e)
	                {   System.err.println(e.getMessage());
	                    System.err.println("Caught exception in MouseHook!");
	                }
	                //System.out.println("terminated ");
	                USER32INST.UnhookWindowsHookEx(hhk);
	                hhk = null;
	        }
	        
	    },"Named thread");
	    threadFinish = false;
	    thrd.start();

	}
	private interface LowLevelMouseProc extends HOOKPROC
	{
	    LRESULT callback(int nCode, WPARAM wParam, MOUSEHOOKSTRUCT lParam);
	}
	public LowLevelMouseProc hookTheMouse() {
	    return new LowLevelMouseProc()
	    {
	        
	        public LRESULT callback(int nCode, WPARAM wParam, MOUSEHOOKSTRUCT info) {
	        	LRESULT result = USER32INST.CallNextHookEx(hhk, nCode, wParam, info.getPointer());
	            if (nCode >= 0)
	            {
	            	int action = wParam.intValue();
	            	//System.out.println(action);
	                switch(action)
	                {
	                    case WM_LBUTTONDOWN:
	                        // do stuff
	                        break;
	                    case WM_RBUTTONDOWN:
	                    	WindowsXPMouse.action.run();
	                        break;
	                    case WM_MBUTTONDOWN:
	                        //do other stuff
	                        break;
	                    case WM_LBUTTONUP:
	                    	WindowsXPMouse.action.run();
	                         break;
	                    case WM_MOUSEMOVE:

	                        break;                         
	                    default:
	                        break;
	                }
	                 /****************************DO NOT CHANGE, this code unhooks mouse *********************************/
	                 if (threadFinish == true)
	                  {   
	                	 //System.out.println("post quit");
	                     USER32INST.PostQuitMessage(0);
	                  }
	                /***************************END OF UNCHANGABLE *******************************************************/
	            }
	            return result;
	        }
	    };
	}
	public static class MOUSEHOOKSTRUCT extends Structure
	{
	    public static class ByReference extends MOUSEHOOKSTRUCT implements Structure.ByReference {};
	    public POINT pt;
	    public HWND hwnd;
	    public int wHitTestCode;
	    public ULONG_PTR dwExtraInfo;
	}
	
	public static void main(String[] args)
	{
	}

}
