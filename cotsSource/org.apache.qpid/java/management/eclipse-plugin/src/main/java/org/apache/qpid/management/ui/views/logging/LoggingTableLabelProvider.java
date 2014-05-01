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
package org.apache.qpid.management.ui.views.logging;

import javax.management.openmbean.CompositeDataSupport;

import org.apache.qpid.management.common.mbeans.LoggingManagement;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;

/**
 * Label Provider class for the LoggingManagement table viewers
 */
public class LoggingTableLabelProvider extends LabelProvider implements ITableLabelProvider
{
    private static final String LOGGER_NAME = LoggingManagement.COMPOSITE_ITEM_NAMES[0];
    private static final String LOGGER_LEVEL = LoggingManagement.COMPOSITE_ITEM_NAMES[1];
    
    @Override
    public String getColumnText(Object element, int columnIndex)
    {
        switch (columnIndex)
        {
            case 0 : // logger name column 
                return (String) ((CompositeDataSupport) element).get(LOGGER_NAME);
            case 1 : // logger level column 
                return (String) ((CompositeDataSupport) element).get(LOGGER_LEVEL);
            default :
                return "-";
        }
    }

    @Override
    public Image getColumnImage(Object element, int columnIndex)
    {
        return null;
    }


}
