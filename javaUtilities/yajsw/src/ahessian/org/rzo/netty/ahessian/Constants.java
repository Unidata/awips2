package org.rzo.netty.ahessian;

import org.jboss.netty.logging.InternalLogger;
import org.jboss.netty.logging.InternalLoggerFactory;

public interface Constants
{
	public static final String HEADER_STRING = "H";
	public static final Integer GROUP_HEADER_KEY = 0;
	public static final Integer CALL_ID_HEADER_KEY = 1;
	public static final Integer SERVICE_ID_HEADER_KEY = 2;
	
	public static final Integer CALLBACK_ID_HEADER_KEY = 3;
	public static final Integer CALLBACK_METHOD_HEADER_KEY = 4;
	public static final Integer CALLBACK_ARGS_HEADER_KEY = 5;
	public static final Integer CALLBACK_DONE_HEADER_KEY = 6;
	
	public static final Integer COMPLETED_HEADER_KEY = 7;
	public static final Integer HAS_SESSION_FILTER_HEADER_KEY = 8;
	
	public static final int IGROUP_HEADER_KEY = 0;
	public static final int ICALL_ID_HEADER_KEY = 1;
	public static final int ISERVICE_ID_HEADER_KEY = 2;
	
	public static final int ICALLBACK_ID_HEADER_KEY = 3;
	public static final int ICALLBACK_METHOD_HEADER_KEY = 4;
	public static final int ICALLBACK_ARGS_HEADER_KEY = 5;
	public static final int ICALLBACK_DONE_HEADER_KEY = 6;
	
	public static final int ICOMPLETED_HEADER_KEY = 7;
	public static final int IHAS_SESSION_FILTER_HEADER_KEY = 8;
	
	
	public static final InternalLogger ahessianLogger =
        InternalLoggerFactory.getInstance("ahessian");

	
}
