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

// TODO: Auto-generated Javadoc
/**
 * The Interface Keyboard.
 */
public interface Keyboard
{

	/** The Constant MOD_ALT. */
	static final int	MOD_ALT		= 1;

	/** The Constant MOD_CONTROL. */
	static final int	MOD_CONTROL	= 2;

	/** The Constant MOD_SHIFT. */
	static final int	MOD_SHIFT	= 4;

	/** The Constant MOD_WIN. */
	static final int	MOD_WIN		= 8;

	/**
	 * The Interface HotKeyListner.
	 */
	public interface HotKeyListner
	{

		/**
		 * Key pressed.
		 */
		public void keyPressed();
	}

	/**
	 * Register hotkey.
	 * 
	 * @param listner
	 *            the listner
	 * @param mod
	 *            the mod
	 * @param key
	 *            the key
	 */
	public void registerHotkey(HotKeyListner listner, int mod, int key);

	/**
	 * Unregister hot key.
	 * 
	 * @param listner
	 *            the listner
	 */
	public void unregisterHotKey(HotKeyListner listner);

}
