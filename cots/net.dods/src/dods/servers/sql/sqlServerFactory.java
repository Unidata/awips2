/////////////////////////////////////////////////////////////////////////////
// Copyright (c) 1999, University of Rhode Island
// ALL RIGHTS RESERVED.
//
// Please read the full copyright notice in the file COPYRIGHT
// in this directory.
//
// Author: James Gallagher <jgallagher@gso.uri.edu>
//
/////////////////////////////////////////////////////////////////////////////
//
// $Log: sqlServerFactory.java,v $
// Revision 1.1.6.1  2004/02/04 22:30:29  ndp
// Add debugging switch 'sqlRead' and
// repaired problems with the read methods
// for sqlF64 and sqlF32.
//
// Revision 1.1  2001/01/08 20:03:08  ndp
// Reorganizing source tree...
//
// Revision 1.1  2000/02/17 16:33:46  ndp
// Added the beginings of the sql classes
//
// Revision 1.2  1999/09/10 22:43:08  ndp
// *** empty log message ***
//
// Revision 1.1  1999/09/09 00:09:35  ndp
// Moved ServerFactory to test/test_ServerFactor
//
// Revision 1.3  1999/09/09 00:07:13  jimg
// Removed accidentally
//
// Revision 1.1  1999/09/02 23:36:53  ndp
// Moved
//
// Revision 1.2  1999/07/21 20:38:53  jimg
// Added methods for int16, UInt16 and Float32.
//

package dods.servers.sql;

import dods.dap.*;

/** The default server-side Factory for BaseType objects.
 @version $Revision: 1.1.6.1 $
 @author jhrg
 @see BaseTypeFactory */
public class sqlServerFactory implements BaseTypeFactory {
    /**
     * Construct a new DByte.
     * @return the new DByte
     */
    public DByte newDByte() {
        return new sqlByte();
    }

    /**
     * Construct a new DByte with name n.
     * @param n the variable name
     * @return the new DByte
     */
    public DByte newDByte(String n) {
        return new sqlByte(n);
    }

    /**
     * Construct a new DInt16.
     * @return the new DInt16
     */
    public DInt16 newDInt16() {
        return new sqlI16();
    }

    /**
     * Construct a new DInt16 with name n.
     * @param n the variable name
     * @return the new DInt16
     */
    public DInt16 newDInt16(String n) {
        return new sqlI16(n);
    }

    /**
     * Construct a new DUInt16.
     * @return the new DUInt16
     */
    public DUInt16 newDUInt16() {
        return new sqlUI16();
    }

    /**
     * Construct a new DUInt16 with name n.
     * @param n the variable name
     * @return the new DUInt16
     */
    public DUInt16 newDUInt16(String n) {
        return new sqlUI16(n);
    }

    /**
     * Construct a new DInt32.
     * @return the new DInt32
     */
    public DInt32 newDInt32() {
        return new sqlI32();
    }

    /**
     * Construct a new DInt32 with name n.
     * @param n the variable name
     * @return the new DInt32
     */
    public DInt32 newDInt32(String n) {
        return new sqlI32(n);
    }

    /**
     * Construct a new DUInt32.
     * @return the new DUInt32
     */
    public DUInt32 newDUInt32() {
        return new sqlUI32();
    }

    /**
     * Construct a new DUInt32 with name n.
     * @param n the variable name
     * @return the new DUInt32
     */
    public DUInt32 newDUInt32(String n) {
        return new sqlUI32(n);
    }

    /**
     * Construct a new DFloat32.
     * @return the new DFloat32
     */
    public DFloat32 newDFloat32() {
        return new sqlF32();
    }

    /**
     * Construct a new DFloat32 with name n.
     * @param n the variable name
     * @return the new DFloat32
     */
    public DFloat32 newDFloat32(String n) {
        return new sqlF32(n);
    }

    /**
     * Construct a new DFloat64.
     * @return the new DFloat64
     */
    public DFloat64 newDFloat64() {
        return new sqlF64();
    }

    /**
     * Construct a new DFloat64 with name n.
     * @param n the variable name
     * @return the new DFloat64
     */
    public DFloat64 newDFloat64(String n) {
        return new sqlF64(n);
    }

    /**
     * Construct a new DString.
     * @return the new DString
     */
    public DString newDString() {
        return new sqlString();
    }

    /**
     * Construct a new DString with name n.
     * @param n the variable name
     * @return the new DString
     */
    public DString newDString(String n) {
        return new sqlString(n);
    }

    /**
     * Construct a new DURL.
     * @return the new DURL
     */
    public DURL newDURL() {
        return new sqlURL();
    }

    /**
     * Construct a new DURL with name n.
     * @param n the variable name
     * @return the new DURL
     */
    public DURL newDURL(String n) {
        return new sqlURL(n);
    }

    /**
     * Construct a new DArray.
     * @return the new DArray
     */
    public DArray newDArray() {
        return new sqlArray();
    }

    /**
     * Construct a new DArray with name n.
     * @param n the variable name
     * @return the new DArray
     */
    public DArray newDArray(String n) {
        return new sqlArray(n);
    }

    /**
     * Construct a new DList.
     * @return the new DList
     */
    public DList newDList() {
        return new sqlList();
    }

    /**
     * Construct a new DList with name n.
     * @param n the variable name
     * @return the new DList
     */
    public DList newDList(String n) {
        return new sqlList(n);
    }

    /**
     * Construct a new DGrid.
     * @return the new DGrid
     */
    public DGrid newDGrid() {
        return new sqlGrid();
    }

    /**
     * Construct a new DGrid with name n.
     * @param n the variable name
     * @return the new DGrid
     */
    public DGrid newDGrid(String n) {
        return new sqlGrid(n);
    }

    /**
     * Construct a new DStructure.
     * @return the new DStructure
     */
    public DStructure newDStructure() {
        return new sqlStruct();
    }

    /**
     * Construct a new DStructure with name n.
     * @param n the variable name
     * @return the new DStructure
     */
    public DStructure newDStructure(String n) {
        return new sqlStruct(n);
    }

    /**
     * Construct a new DSequence.
     * @return the new DSequence
     */
    public DSequence newDSequence() {
        return new sqlSeq();
    }

    /**
     * Construct a new DSequence with name n.
     * @param n the variable name
     * @return the new DSequence
     */
    public DSequence newDSequence(String n) {
        return new sqlSeq(n);
    }
}
