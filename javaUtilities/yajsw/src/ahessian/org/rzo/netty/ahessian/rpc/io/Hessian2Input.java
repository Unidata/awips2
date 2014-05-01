/*
 * Copyright (c) 2001-2008 Caucho Technology, Inc.  All rights reserved.
 *
 * The Apache Software License, Version 1.1
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution, if
 *    any, must include the following acknowlegement:
 *       "This product includes software developed by the
 *        Caucho Technology (http://www.caucho.com/)."
 *    Alternately, this acknowlegement may appear in the software itself,
 *    if and wherever such third-party acknowlegements normally appear.
 *
 * 4. The names "Hessian", "Resin", and "Caucho" must not be used to
 *    endorse or promote products derived from this software without prior
 *    written permission. For written permission, please contact
 *    info@caucho.com.
 *
 * 5. Products derived from this software may not be called "Resin"
 *    nor may "Resin" appear in their names without prior written
 *    permission of Caucho Technology.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL CAUCHO TECHNOLOGY OR ITS CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
 * OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
 * OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
 * IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * @author Scott Ferguson
 */

package org.rzo.netty.ahessian.rpc.io;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

import org.rzo.netty.ahessian.Constants;
import org.rzo.netty.ahessian.io.InputStreamBuffer;


/**
 * The Class Hessian2Input.
 */
public class Hessian2Input
  extends  com.caucho.hessian4.io.Hessian2Input 
{

	boolean _closed = false;
	/**
	 * Instantiates a new hessian2 input.
	 * 
	 * @param is the is
	 */
	volatile InputStreamBuffer _isb;
	public Hessian2Input(InputStream is)
	{
		super(is);
		_isb = (InputStreamBuffer) is;
	}
	
	public boolean bufferEmpty()
	{
		if (_isb == null)
			return true;
		try
		{
			return _isb.available() == 0 && _length <= _offset;
		}
		catch (IOException e)
		{
			Constants.ahessianLogger.warn("", e);
			return true;
		}
	}
	
	public void close() throws IOException
	{
		_closed = true;
		super.close();
	}
	
	public boolean isClosed()
	{
		return _closed;
	}

	/**
	 * Read headers.
	 * 
	 * @return the map
	 */
	public Map readHeaders()
	{
		Map result = new HashMap();
		String header = null;
		try
		{
			header = readHeader();
		}
		catch (IOException e)
		{
			Constants.ahessianLogger.warn("", e);
		}
		while (header != null)
		{
			try
			{
				result.put(header, readObject());
				header = readHeader();
			}
			catch (IOException e)
			{
				Constants.ahessianLogger.warn("", e);
				header = null;
			}
		}
		return result;			
	}
	
	public InputStream getInputStream()
	{
		return _isb;
	}
	



}
