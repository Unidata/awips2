/*****************************************************************************************
 * COPYRIGHT (c), 2006-2008, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/
package jep;

/**
 * Interface representing a Java object that can be transformed into a numpy
 * array.  
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#         Engineer        Description
 * ------------	----------	-----------	--------------------------
 * Apr 4, 2008                  njensen         Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public interface INumpyable {

    /**
     * Gets an Object[] representation of the object to transform into numpy.
     * Each index in the Object[] should be another array of the primitive type,
     * e.g. {float[], float[]}.  The result in python will then be a python
     * list, e.g. [numpy.ndarray(dtype=float32), numpy.ndarray(dtype=float32)].
     * 
     * @return
     */
    public Object[] getNumpy();

    /**
     *  Gets the x dimension of the arrays returned by getNumpy().
     */
    public int getNumpyX();

    /**
     *  Gets the y dimension of hte arrays returned by getNumpy().
     */
    public int getNumpyY();

}
