/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *
 */
package org.apache.qpid.management.ui;

/**
 * Contains constants for the application
 * @author Bhupendra Bhardwaj
 *
 */
public class Constants
{
    public final static String APPLICATION_NAME = "Qpid JMX Management Console";
    public static final String DEFAULT_DOMAIN = "org.apache.qpid";
    
    public final static String ACTION_REMOVE_MBEANNODE = "Remove from list";
    public final static String VALUE = "value";
    public final static String TYPE  = "type";
    public final static String VERSION  = "version";
    public final static String NODE_TYPE_SERVER    = "server";
    public final static String NODE_TYPE_MBEANTYPE = "mbeantype";
    // currently used only for virtual host instances, but will work as general also
    public final static String NODE_TYPE_TYPEINSTANCE = "mbeantype_instance";
    public final static String MBEAN      = "mbean";
    public final static String ATTRIBUTE = "Attribute";
    public final static String ATTRIBUTES = "Attributes";
    public final static String NOTIFICATIONS = "Notifications";
    public final static String RESULT = "Result";
    public final static String VIRTUAL_HOST = "VirtualHost";
    public final static String DEFAULT_VH = "Default";
    public final static String DEFAULT_USERNAME = "guest";
    public final static String DEFAULT_PASSWORD = "guest";
    
    public final static String USERNAME = "Username";
    public final static String PASSWORD = "Password";
    
    // Attributes and operations are used to customize the GUI for Qpid. If these are changes in the
    // Qpid server, then these should be updated accordingly
    public final static String ATTRIBUTE_QUEUE_OWNER = "owner";
    public final static String ATTRIBUTE_QUEUE_DEPTH = "QueueDepth";
    public final static String ATTRIBUTE_QUEUE_CONSUMERCOUNT = "ActiveConsumerCount";    
    public final static String OPERATION_CREATE_QUEUE = "createNewQueue";
    public final static String OPERATION_CREATE_BINDING = "createNewBinding";
    public final static String OPERATION_MOVE_MESSAGES = "moveMessages";
    
    public final static String OPERATION_CREATEUSER = "createUser";
    public final static String OPERATION_DELETEUSER = "deleteUser";
    public final static String OPERATION_VIEWUSERS = "viewUsers";
    public final static String OPERATION_PARAM_USERNAME = "username";
    
    public final static String OPERATION_SUCCESSFUL = "Operation successful";
    public final static String OPERATION_UNSUCCESSFUL = "Operation unsuccessful";
    
    public final static String ALL = "All";
    
    public final static String NAVIGATION_ROOT = "Qpid Connections";
    public final static String DESCRIPTION = " Description";
    
    public final static String ADMIN_MBEAN_TYPE = "UserManagement";
    public final static String QUEUE  = "Queue";
    public final static String CONNECTION ="Connection";
    public final static String EXCHANGE = "Exchange";
    public final static String EXCHANGE_TYPE = "ExchangeType";
    public final static String[] DEFAULT_EXCHANGE_TYPE_VALUES = {"direct", "fanout", "headers", "topic"};
    public final static String[] BOOLEAN_TYPE_VALUES = {"false", "true"};
    public final static String[] ATTRIBUTE_TABLE_TITLES = {"Attribute Name", "Value"};  
    
    public final static String ACTION_ADDSERVER = "New Connection";
    public final static String ACTION_RECONNECT = "Reconnect";
    public final static String ACTION_CLOSE = "Close Connection";
    public final static String ACTION_EDITATTRIBUTE = "Edit Attribute";
    public final static String ACTION_LOGIN = "Login";
    
    public final static String QUEUE_SORT_BY_NAME = "Queue Name";
    public final static String QUEUE_SORT_BY_DEPTH = "Queue Depth";
    public final static String QUEUE_SORT_BY_CONSUMERCOUNT = "Consumer Count";
    public final static String QUEUE_SHOW_TEMP_QUEUES= "show temporary queues";
    
    public final static String SUBSCRIBE_BUTTON   = "Subscribe";
    public final static String UNSUBSCRIBE_BUTTON = "Unsubscribe";
    
    
    public final static String SUCCESS_IMAGE = "SuccessImage";
    public final static String FAILURE_IMAGE = "FailureImage";
    public final static String CONSOLE_IMAGE = "ConsoleImage";
    public final static String CLOSED_FOLDER_IMAGE = "ClosedFolderImage";
    public final static String OPEN_FOLDER_IMAGE = "OpenFolderImage";
    public final static String MBEAN_IMAGE = "MBeanImage";
    public final static String NOTIFICATION_IMAGE = "NotificationImage";
    public final static String LOGGING_MANAGEMENT_IMAGE = "LoggingManagementImage";
    public final static String USER_MANAGEMENT_IMAGE = "UserManagementImage";
    public final static String CONFIGURATION_MANAGEMENT_IMAGE = "ConfigurationManagementImage";
    public final static String SERVER_INFO_IMAGE = "ServerInfoImage";
    public final static String VHOST_MANAGER_IMAGE = "VhostManagerImage";
    
    public final static String FONT_BUTTON = "ButtonFont";
    public final static String FONT_BOLD = "BoldFont";
    public final static String FONT_ITALIC = "ItalicFont";
    public final static String FONT_TABLE_CELL = "TableCellFont";
    public final static String FONT_NORMAL = "Normal";
    
    public final static String BUTTON_DETAILS = "Details";
    public final static String BUTTON_EDIT_ATTRIBUTE = "Edit";
    public final static String BUTTON_REFRESH = "Refresh";
    public final static String BUTTON_GRAPH = "Graph";
    public final static int TIMER_INTERVAL = 5000;
    public final static String BUTTON_EXECUTE = "Execute";
    public final static String BUTTON_CLEAR = "Clear";
    public final static String BUTTON_CONNECT = "Connect";
    public final static String BUTTON_CANCEL = "Cancel";
    public final static String BUTTON_UPDATE = "Update";
    
    
    public final static int OPERATION_IMPACT_INFO    = 0;
    public final static int OPERATION_IMPACT_ACTION  = 1;
    public final static int OPERATION_IMPACT_ACTIONINFO  = 2;
    public final static int OPERATION_IMPACT_UNKNOWN = 3;
    
    public final static String ERROR_SERVER_CONNECTION = "Server Connection Failed";
    public final static String INFO_PROTOCOL = "Please select the protocol";
    public final static String INFO_HOST_ADDRESS = "Please enter the host address";
    public final static String INFO_HOST_PORT = "Please enter the port number";
    public final static String INFO_USERNAME = "Please enter the " + USERNAME;
    public final static String INFO_PASSWORD = "Please enter the " + PASSWORD;
    
}
