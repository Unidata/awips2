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

// TODO: Auto-generated Javadoc
/**
 * The Class PdhBufferedCounter.
 */
public class PdhBufferedCounter implements PdhCounter
{

	/** The _counter. */
	PdhCounter	_counter;

	/** The i buff. */
	int[]		iBuff;

	/** The d buff. */
	double[]	dBuff;

	/** The _frequency. */
	long		_frequency;

	/** The _last add. */
	long		_lastAdd;

	/** The _next add. */
	long		_nextAdd;

	/** The _head. */
	int			_head	= -1;

	/** The _tail. */
	int			_tail	= -1;

	/** The _count. */
	int			_count	= 0;

	/** The _i avg. */
	int			_iAvg;

	/** The _d avg. */
	double		_dAvg;

	/**
	 * Instantiates a new pdh buffered counter.
	 * 
	 * @param counter
	 *            the counter
	 * @param buffSize
	 *            the buff size
	 * @param frequency
	 *            the frequency
	 * @param valueType
	 *            the value type
	 */
	public PdhBufferedCounter(PdhCounter counter, int buffSize, long frequency, Class valueType)
	{
		_counter = counter;
		_frequency = frequency;
		if (valueType.equals(int.class))
		{
			iBuff = new int[buffSize];
		}
		else if (valueType.equals(double.class))
		{
			dBuff = new double[buffSize];
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.os.ms.win.xp.PdhCounter#getDoubleValue()
	 */
	public double getDoubleValue()
	{
		return _counter.getDoubleValue();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.os.ms.win.xp.PdhCounter#getIntValue()
	 */
	public int getIntValue()
	{
		return _counter.getIntValue();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.os.ms.win.xp.PdhCounter#isValid()
	 */
	public boolean isValid()
	{
		return _counter.isValid();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.os.ms.win.xp.PdhCounter#close()
	 */
	public void close()
	{
		_counter.close();
	}

	/**
	 * Adds the.
	 */
	private void add()
	{
		if (iBuff != null)
		{
			if (_count < iBuff.length)
				_count++;
			_head = (_head + 1) % iBuff.length;
			if (_head == _tail || _tail == -1)
				_tail = (_tail + 1) % iBuff.length;
			iBuff[_head] = _counter.getIntValue();
			_iAvg = (_iAvg * (_count - 1) + iBuff[_head]) / _count;
		}
		else if (dBuff != null)
		{
			if (_count < dBuff.length)
				_count++;
			_head = (_head + 1) % dBuff.length;
			if (_head == _tail || _tail == -1)
				_tail = (_tail + 1) % dBuff.length;
			dBuff[_head] = _counter.getDoubleValue();
			_dAvg = (_dAvg * (_count - 1) + dBuff[_head]) / _count;
		}
	}

	/**
	 * Tick.
	 */
	public void tick()
	{
		long t = System.currentTimeMillis();
		if (t >= _nextAdd)
		{
			add();
			_lastAdd = t;
			_nextAdd = _lastAdd + _frequency;
		}
		// System.out.println(t-System.currentTimeMillis());
	}

	/**
	 * Gets the int avg.
	 * 
	 * @return the int avg
	 */
	public int getIntAvg()
	{
		return _iAvg;
	}

	/**
	 * Gets the double avg.
	 * 
	 * @return the double avg
	 */
	public double getDoubleAvg()
	{
		return _dAvg;
	}

	/**
	 * Gets the int head.
	 * 
	 * @return the int head
	 */
	public int getIntHead()
	{
		if (iBuff != null && _head >= 0)
			return iBuff[_head];
		else
			return -1;

	}

	/**
	 * Gets the double head.
	 * 
	 * @return the double head
	 */
	public double getDoubleHead()
	{
		if (dBuff != null && _head >= 0)
			return dBuff[_head];
		else
			return -1;

	}

	/**
	 * Gets the int buffer.
	 * 
	 * @return the int buffer
	 */
	public int[] getIntBuffer()
	{
		return iBuff;
	}

	/**
	 * Gets the double buffer.
	 * 
	 * @return the double buffer
	 */
	public double[] getDoubleBuffer()
	{
		return dBuff;
	}

}
