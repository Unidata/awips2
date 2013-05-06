package org.rzo.netty.ahessian.application.jmx.remote.service;

import java.io.IOException;

import javax.management.ObjectName;

import com.caucho.hessian4.io.AbstractHessianOutput;
import com.caucho.hessian4.io.AbstractSerializer;

public class ObjectNameSerializer extends AbstractSerializer
{

	public void writeInstance(Object obj, AbstractHessianOutput out) throws IOException
	{
		ObjectName on = (ObjectName)obj;
		out.writeString(on.getCanonicalName());
	}
	
	protected void writeDefinition20(Class<?> cl,
            AbstractHessianOutput out)
throws IOException
{
  out.writeInt(0);
}
}
