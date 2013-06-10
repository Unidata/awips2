package org.rzo.netty.ahessian.rpc.message;

import java.util.Map;

import com.caucho.hessian4.io.Deserializer;
import com.caucho.hessian4.io.HessianProtocolException;
import com.caucho.hessian4.io.Serializer;
import com.caucho.hessian4.io.SerializerFactory;

public class MappingSerializerFactory extends SerializerFactory
{
	Map<String, String> _serializers;
	Map<String, String> _deserializers;
	
	
	public MappingSerializerFactory(Map<String, String> serializers, Map<String, String> deserializers)
	{
		_serializers = serializers;
		_deserializers = deserializers;
	}
	
	@Override
	protected Deserializer loadDeserializer(Class cl) throws HessianProtocolException
	{
		String type = getType(cl, _deserializers);
		return  (Deserializer) instantiate(type);
		
	}
	
	protected Serializer loadSerializer(Class cl) throws HessianProtocolException
    {
		String type = getType(cl, _serializers);
		return  (Serializer) instantiate(type);
    }
	
	private String getType(Class cl, Map<String, String> mapping)
	{
		if (mapping == null || cl == null)
			return null;
		return mapping.get(cl.getName());
	}
	
	private Object instantiate(String type)
	{
		if (type == null)
			return null;
		try
		{
			Class clazz =  Class.forName(type);
			Object result = clazz.newInstance();
			return result;
		}
		catch (Exception ex)
		{
			ex.printStackTrace();
			return null;
		}
		
	}
}
