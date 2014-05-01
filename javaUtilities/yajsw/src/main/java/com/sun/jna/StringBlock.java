package com.sun.jna;

/*
 * A string block consists of a null-terminated block of null-terminated strings. 
 * Note that an ANSI environment block is terminated by two zero bytes: 
 * one for the last string, one more to terminate the block. 
 * A Unicode environment block is terminated by four zero bytes: 
 * two for the last string, two more to terminate the block.
 */

public class StringBlock extends Memory
{
	private boolean	wide;

	/** Create a native array of strings. */
	public StringBlock(String[] strings)
	{
		this(strings, false);
	}

	/** Create a native block of strings. */
	public StringBlock(String[] strings, boolean wide)
	{
		this((Object[]) strings, wide);
	}

	/** Create a native block of wide strings. */
	public StringBlock(WString[] strings)
	{
		this(strings, true);
	}

	private StringBlock(Object[] strings, boolean wide)
	{
		super(calculateLength(strings, wide));
		this.wide = wide;
		int offset = 0;
		for (int i = 0; i < strings.length; i++)
		{
			if (strings[i] != null)
			{
				setString(offset, strings[i].toString(), wide);
				if (wide)
					offset += (strings[i].toString().length()) * Native.WCHAR_SIZE;
				else
					offset += (strings[i].toString().getBytes().length);
				setString(offset, "\0", wide);
				if (wide)
					offset += Native.WCHAR_SIZE;
				else
					offset += 1;

			}
		}
		setByte(offset, (byte) 0);
		if (wide)
			setByte(offset + 1, (byte) 0);
	}

	private static long calculateLength(Object[] strings, boolean wide)
	{
		int result = 0;
		if (wide)
		{
			for (Object string : strings)
				result += (string.toString().length() + 1) * Native.WCHAR_SIZE;
			result += Native.WCHAR_SIZE;
		}
		else
		{
			for (Object string : strings)
				result += (string.toString().getBytes().length + 1);
			result += 1;
		}
		return result;
	}

}
