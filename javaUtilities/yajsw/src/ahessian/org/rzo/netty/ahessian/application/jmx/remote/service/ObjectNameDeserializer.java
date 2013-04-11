package org.rzo.netty.ahessian.application.jmx.remote.service;

import java.io.IOException;

import javax.management.ObjectName;

import com.caucho.hessian4.io.AbstractDeserializer;
import com.caucho.hessian4.io.AbstractHessianInput;

public class ObjectNameDeserializer extends AbstractDeserializer
{

	  public Object readObject(AbstractHessianInput in, 
              Object []fields)
throws IOException
{
		String on = in.readString();
		try
		{
			Object result = new ObjectName(on);
			in.addRef(result);
			return result;
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		return null;
	}

}
