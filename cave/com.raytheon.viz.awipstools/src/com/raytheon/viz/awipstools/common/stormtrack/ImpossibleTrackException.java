/*****************************************************************************************
 * COPYRIGHT (c), 2007, RAYTHEON COMPANY
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
package com.raytheon.viz.awipstools.common.stormtrack;

import com.raytheon.uf.viz.core.exception.VizException;

/**TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05-28-2010   #6042      bkowal      Renaming ImpossibleStormException to
 *                                     ImpossibleTrackException.
 * 
 * </pre>
 *
 * @author bkowal
 * @version 1.0	
 */

public class ImpossibleTrackException extends VizException {

	private static final long serialVersionUID = -7171916545937661879L;

	/**
	 * 
	 */
	public ImpossibleTrackException()
	{
		super();
	}
	
	/**
	 * @param message
	 * @param cause
	 */
	public ImpossibleTrackException(String message, Throwable cause)
	{
		super(message, cause);
	}
	
	/**
	 * @param message
	 */
	public ImpossibleTrackException(String message)
	{
		super(message);
	}
	
	/**
	 * @param cause
	 */
	public ImpossibleTrackException(Throwable cause)
	{
		super(cause);
	}
}
