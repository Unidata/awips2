/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/

package com.raytheon.uf.viz.core.rsc;

import org.eclipse.swt.widgets.Event;

/**
 * Interface to register a mouse event handler with the map
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 7/1/06                   chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public interface IInputHandler {

    public static enum InputPriority {
        LOWEST(0), RESOURCE(3), SYSTEM_RESOURCE(4), PART(5), PERSPECTIVE(6), WINDOW(
                7);

        public Integer value;

        private InputPriority(int value) {
            this.value = value;
        }

        /**
         * Get a priority from a value, if value is not valid, returns RESOUCE
         * 
         * @param value
         * @return
         */
        public static InputPriority fromValue(int value) {
            for (InputPriority p : values()) {
                if (p.value == value) {
                    return p;
                }
            }
            return RESOURCE;
        }
    }

    /**
     * Handle a mouse down event
     * 
     * @param x
     *            the x screen coordinate
     * @param y
     *            the y screen coordinate
     * @param mouseButton
     *            the button held down
     * @return true if other handlers should be pre-empted
     */
    public abstract boolean handleMouseDown(int x, int y, int mouseButton);

    /**
     * Handle a mouse down move event
     * 
     * @param x
     *            the x screen coordinate
     * @param y
     *            the y screen coordinate
     * @param mouseButton
     *            the button held down
     * @return true if other handlers should be pre-empted
     */
    public abstract boolean handleMouseDownMove(int x, int y, int mouseButton);

    /**
     * Handle a mouse up event
     * 
     * @param x
     *            the x screen coordinate
     * @param y
     *            the y screen coordinate
     * @param mouseButton
     *            the button held down
     * @return true if other handlers should be pre-empted
     */
    public abstract boolean handleMouseUp(int x, int y, int mouseButton);

    /**
     * Handle a mouse hover event
     * 
     * @param x
     *            the x screen coordinate
     * @param y
     *            the y screen coordinate
     * @return true if other handlers should be pre-empted
     */
    public abstract boolean handleMouseHover(int x, int y);

    /**
     * Handle a mouse move event
     * 
     * @param x
     *            the x screen coordinate
     * @param y
     *            the y screen coordinate
     * @return true if other handlers should be pre-empted
     */
    public abstract boolean handleMouseMove(int x, int y);

    /**
     * Handle a double click event
     * 
     * @param x
     *            the x screen coordinate
     * @param y
     *            the y screen coordinate
     * @return true if other handlers should be pre-empted
     */
    public abstract boolean handleDoubleClick(int x, int y, int button);

    /**
     * Handle a mouse wheel event
     * 
     * @param event
     *            the wheel event
     * @param x
     *            the x screen coordinate
     * @param y
     *            the y screen coordinate
     * @return true if the other handlers should be pre-empted
     */
    public abstract boolean handleMouseWheel(Event event, int x, int y);

    /**
     * Handle the mouse exit event
     * 
     * @param event
     * @return
     */
    public abstract boolean handleMouseExit(Event event);

    /**
     * Handle the mouse enter event
     * 
     * @param event
     * @return
     */
    public abstract boolean handleMouseEnter(Event event);

    /**
     * Handle a key down code
     * 
     * The key value can be cast to 'char' for regular characters, and compared
     * to the SWT constants for keys such as tab.
     * 
     * @param keyCode
     *            the key pressed on the keyboard
     * @return true if the other handlers should be pre-empted
     */
    public abstract boolean handleKeyDown(int keyCode);

    /**
     * Handle a key up code
     * 
     * The key value can be cast to 'char' for regular characters, and compared
     * to the SWT constants for keys such as tab.
     * 
     * @param keyCode
     *            the key released on the keyboard
     * @return true if the other handlers should be pre-empted
     */
    public abstract boolean handleKeyUp(int keyCode);

}
