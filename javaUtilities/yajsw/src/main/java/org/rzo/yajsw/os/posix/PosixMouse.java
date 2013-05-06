package org.rzo.yajsw.os.posix;

import java.util.concurrent.Executor;

import org.rzo.yajsw.os.Mouse;

import com.sun.jna.NativeLong;
import com.sun.jna.platform.unix.X11;
import com.sun.jna.platform.unix.X11.XEvent;

public class PosixMouse implements Mouse
{
    //static final Display display = X11.INSTANCE.XOpenDisplay(null);
    static final XEvent xevent = new XEvent();
    static boolean _registered = false;
    static Thread thread;
    static boolean stop;
    
    public interface Xlib extends X11 {

        int XGrabKey(Display display, int keycode, NativeLong modifiers, Window grab_window, boolean owner_events, int pointer_mode, int keyboard_mode);
          /*
          Display *display;
          int keycode;
          unsigned int modifiers;
          Window grab_window;
          Bool owner_events;
          int pointer_mode, keyboard_mode;
          */
       int XGrabButton(Display display, NativeLong button, NativeLong modifiers, Window grab_window, boolean owner_events, NativeLong event_mask, 
                int pointer_mode, int keyboard_mode, Window confine_to, Cursor cursor);
                /*
      Display *display;
      unsigned int button;
      unsigned int modifiers;
      Window grab_window;
      Bool owner_events;
      unsigned int event_mask;	
      int pointer_mode, keyboard_mode;
      Window confine_to; 
      Cursor cursor; 
      
       
       int XGrabPointer(display, grab_window, owner_events, event_mask, pointer_mode,
               keyboard_mode, confine_to, cursor, time)
      Display *display;
      Window grab_window;
      Bool owner_events;
      unsigned int event_mask;	
      int pointer_mode, keyboard_mode; 
      Window confine_to; 
      Cursor cursor; 
      Time time; 
      */
    }

	
	private static Mouse	instance;

	public static Mouse instance()
	{
		System.out.println("posix mouse");
		if (instance == null)
			instance = new PosixMouse();
		return instance;
	}

	public void registerMouseUpListner(final Runnable listner, Executor executor)
	{
		/*
		if (!stop)
			return;
		stop = false;
		thread = new Thread(new Runnable()
		{
			public void run()
			{
				System.out.println("start mouse listener ");
				try
				{
				while (!isStop())
				{
					int r = X11.INSTANCE.XNextEvent(display, xevent);
					System.out.println("xnextevent "+r);
					if (xevent.type == X11.ButtonRelease && !stop)
						listner.run();
				}
				}
				catch (Exception ex)
				{
					ex.printStackTrace();
				}
				System.out.println("end mouse listener ");
			}
			
		});
		thread.start();
		*/
	}
	
	private boolean isStop()
	{
		return stop;
	}

	public void unregisterMouseUpListner()
	{
		/*
		stop = true;
		if (thread != null)
			thread.interrupt();
			*/
	}

}
