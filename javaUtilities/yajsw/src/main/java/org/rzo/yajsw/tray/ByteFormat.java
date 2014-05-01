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
package org.rzo.yajsw.tray;

/**
 * taken from
 * http://groups.google.com/group/comp.lang.java.help/browse_thread/thread
 * /0db818517ca9de79/b0a55aa19f911204 thanks to Piotr Kobzda Formatter for Bytes
 */
public class ByteFormat
{
	/**
	 * The Enum StorageUnit.
	 */
	public enum StorageUnit
	{

		/** The BYTE. */
		BYTE("B", 1L),
		/** The KILOBYTE. */
		KILOBYTE("KB", 1L << 10),
		/** The MEGABYTE. */
		MEGABYTE("MB", 1L << 20),
		/** The GIGABYTE. */
		GIGABYTE("GB", 1L << 30),
		/** The TERABYTE. */
		TERABYTE("TB", 1L << 40),
		/** The PETABYTE. */
		PETABYTE("PB", 1L << 50),
		/** The EXABYTE. */
		EXABYTE("EB", 1L << 60);

		/** The Constant BASE. */
		public static final StorageUnit	BASE	= BYTE;

		private final String			symbol;
		private final long				divider;		// divider of BASE unit

		/**
		 * Instantiates a new storage unit.
		 * 
		 * @param name
		 *            the name
		 * @param divider
		 *            the divider
		 */
		StorageUnit(String name, long divider)
		{
			this.symbol = name;
			this.divider = divider;
		}

		/**
		 * Of.
		 * 
		 * @param number
		 *            the number
		 * 
		 * @return the storage unit
		 */
		public static StorageUnit of(final long number)
		{
			final long n = number > 0 ? -number : number;
			if (n > -(1L << 10))
			{
				return BYTE;
			}
			else if (n > -(1L << 20))
			{
				return KILOBYTE;
			}
			else if (n > -(1L << 30))
			{
				return MEGABYTE;
			}
			else if (n > -(1L << 40))
			{
				return GIGABYTE;
			}
			else if (n > -(1L << 50))
			{
				return TERABYTE;
			}
			else if (n > -(1L << 60))
			{
				return PETABYTE;
			}
			else
			{ // n >= Long.MIN_VALUE
				return EXABYTE;
			}
		}
	}

	/**
	 * Format.
	 * 
	 * @param number
	 *            the number of bytes
	 * 
	 * @return the formatted string
	 */
	public String format(long number)
	{
		StorageUnit st = StorageUnit.of(number);
		return nf.format((double) number / st.divider) + " " + st.symbol;
	}

	private static java.text.NumberFormat	nf	= java.text.NumberFormat.getInstance();
	static
	{
		nf.setGroupingUsed(false);
		nf.setMinimumFractionDigits(0);
		nf.setMaximumFractionDigits(1);
	}

}
