/////////////////////////////////////////////////////////////////////////////
// Copyright (c) 1999, COAS, Oregon State University
// ALL RIGHTS RESERVED.   U.S. Government Sponsorship acknowledged.
//
// Please read the full copyright notice in the file COPYRIGHT
// in this directory.
//
// Author: Nathan Potter (ndp@oce.orst.edu)
//
//                        College of Oceanic and Atmospheric Scieneces
//                        Oregon State University
//                        104 Ocean. Admin. Bldg.
//                        Corvallis, OR 97331-5503
//
/////////////////////////////////////////////////////////////////////////////

package dods.servers.sql;

import java.io.*;
import java.sql.*;

import dods.dap.*;
import dods.dap.Server.*;

/**
 *
 * @version $Revision: 1.2.6.1 $
 * @author ndp
 * @see BaseType
 */
public class sqlResponse {

    private ResultSet rs;
    private int currentColumn;
    private int maxRowsToSend;


    protected sqlResponse() {
        this(null, 10);
        currentColumn = -1;
    }

    public sqlResponse(ResultSet result) {
        this(result, 10);
        currentColumn = 1;
    }

    public sqlResponse(ResultSet result, int maxRows) {
        rs = result;
        currentColumn = 1;
        maxRowsToSend = maxRows;
    }

    public ResultSet getResultSet() {
        return (rs);
    }

    public void setMaxRows(int mr) {
        maxRowsToSend = mr;
    }

    public int getMaxRows() {
        return (maxRowsToSend);
    }


    public void setCurrentColumn(int cc) {
        currentColumn = cc;
    }

    public int nextColumn() {
        currentColumn++;
        return (currentColumn);
    }

    public int firstColumn() {
        currentColumn = 1;
        return (currentColumn);
    }

    public int getCurrentColumn() {
        return (currentColumn);
    }


}
