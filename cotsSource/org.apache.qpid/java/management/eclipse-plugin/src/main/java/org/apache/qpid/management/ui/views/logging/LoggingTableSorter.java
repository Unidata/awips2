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

import javax.management.openmbean.CompositeData;

import org.apache.qpid.management.common.mbeans.LoggingManagement;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;

/**
 * Sorter class for the Logging Management table viewers.
 */
public class LoggingTableSorter extends ViewerSorter
{
    private static final String LOGGER_NAME = LoggingManagement.COMPOSITE_ITEM_NAMES[0];
    private static final String LOGGER_LEVEL = LoggingManagement.COMPOSITE_ITEM_NAMES[1];
    private static final int ASCENDING = 0;
    private static final int DESCENDING = 1;
    
    private int column;
    private int direction;

    public LoggingTableSorter()
    {
        this.column = 0;
        direction = ASCENDING;
    }

    public void setColumn(int column)
    {
        if (column == this.column)
        {
            // Same column as last sort; toggle the direction
            direction = 1 - direction;
        }
        else
        {
            // New column; do an ascending sort
            this.column = column;
            direction = ASCENDING;
        }
    }

    @Override
    public int compare(Viewer viewer, Object e1, Object e2)
    {
        CompositeData logger1 = (CompositeData) e1;
        CompositeData logger2 = (CompositeData) e2;
        
        int comparison = 0;
        switch(column)
        {
            case 0:
                comparison = String.valueOf(logger1.get(LOGGER_NAME)).compareTo(
                                            String.valueOf(logger2.get(LOGGER_NAME)));
                break;
            case 1:
                comparison = String.valueOf(logger1.get(LOGGER_LEVEL)).compareTo(
                                            String.valueOf(logger2.get(LOGGER_LEVEL)));
                break;
            default:
                comparison = 0;
        }
        // If descending order, flip the direction
        if (direction == DESCENDING)
        {
            comparison = -comparison;
        }
        return comparison;
    }
}
