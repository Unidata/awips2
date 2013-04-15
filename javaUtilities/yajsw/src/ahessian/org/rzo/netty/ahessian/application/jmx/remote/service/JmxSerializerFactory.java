package org.rzo.netty.ahessian.application.jmx.remote.service;

import java.util.HashMap;
import java.util.Map;

import javax.management.ObjectName;

import org.rzo.netty.ahessian.rpc.message.MappingSerializerFactory;

import com.caucho.hessian4.io.ObjectNameDeserializer;
import com.caucho.hessian4.io.StringValueSerializer;

public class JmxSerializerFactory extends MappingSerializerFactory
{
	static Map<String, String> serializers = new HashMap<String, String>();
	static Map<String, String> deserializers = new HashMap<String, String>();

	static
	{
		serializers.put(ObjectName.class.getName(), StringValueSerializer.class.getName());
		deserializers.put(ObjectName.class.getName(), ObjectNameDeserializer.class.getName());		
	}
	
	public JmxSerializerFactory()
	{
		super(serializers, deserializers);
	}


}
