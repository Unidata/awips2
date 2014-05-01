/*
 * PgenCommandManager
 * 
 * Date created: 14 APRIL 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.controls;

import java.util.HashSet;
import java.util.Set;
import java.util.Stack;

/**
 * Manages a list of PgenCommand objects to implement and "undo/redo" feature in 
 * the PGEN drawing tools.  The manager executes commands when received and maintains
 * internal stacks that can be used to "undo" or "redo" a prevoius command.  The manager 
 * will also notify listeners when the size of ither stack has changed.
 * @author sgilbert
 *
 */
public class PgenCommandManager {

	/*
	 * Stack of commands that can be undone
	 */
	private Stack<PgenCommand> undo;
	
	/*
	 * stack of commands that can be re-done
	 */
	private Stack<PgenCommand> redo;
	
	/*
	 * List of command stack listeners
	 */
	private Set<CommandStackListener> listeners;
	
	/**
	 * No-arg Constructor 
	 */
	public PgenCommandManager() {
		
		undo = new Stack<PgenCommand>();
		redo = new Stack<PgenCommand>();
		listeners = new HashSet<CommandStackListener>();
	}
	
	/**
	 * Executes a given PgenCommand and saves it on the "undo" stack if it executes
	 * without exception
	 * @param command PgenCommand to execute
	 */
	public void addCommand( PgenCommand command ) {
		
		try {
			/*
			 * excute command
			 */
			command.execute();
			
			/*
			 * Add command to undo stack and clear out redo stack if it isn't empty
			 */
			undo.push(command);
			if ( ! redo.isEmpty() ) {
				redo.clear();
			}
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		finally {
			// notify listeners
			stacksChanged();
		}
		
	}
	
	/**
	 * Executes the undo() method of the PgenCommand Object on the undo stack,
	 * and then adds the command to the redo stack, if the undo was executed
	 * without exception.
	 */
	public void undo() {
		
		if (undo.isEmpty() ) return;

		/*
		 * Get last command from stack
		 */
		PgenCommand cmd = undo.pop();
		
		/*
		 * Execute command's undo() method and add it to redo stack
		 */
		try {
			cmd.undo();
			redo.push(cmd);
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		finally {
			// notify listeners
			stacksChanged();
		}

	}
	
	/**
	 * Runs the execyte() method from the last command on the redo stack, and
	 * adds it to the undo stack, if it ran withou exception.
	 */
	public void redo() {

		if (redo.isEmpty()) return;

		/*
		 * get command from redo stack
		 */
		PgenCommand cmd = redo.pop();
		
		/*
		 * rund command's execute() method and add to undo stack
		 */
		try {
			cmd.execute();
			undo.push(cmd);
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		finally {
			// notify listeners
			stacksChanged();
		}
		
	}
	
	/**
	 * Clear out both the undo and redo stacks. Remoe all listeners
	 */
	public void flushStacks() {
		undo.clear();
		redo.clear();
		listeners.clear();
	}

	/**
	 * Clear out both the undo and redo stacks.
	 */
	public void clearStacks() {
		undo.clear();
		redo.clear();
	}
	
	/**
	 * Register a new stack listener
	 * @param clisten command stack listener
	 */
	public void addStackListener(CommandStackListener clisten) {
		listeners.add(clisten);
		//notify new listener
		clisten.stacksUpdated(undo.size(), redo.size());
	}
	
	/**
	 * Remove the stack listener from the list of listeners
	 * @param clisten command stack listener
	 */
	public void removeStackListener(CommandStackListener clisten) {
		listeners.remove(clisten);
	}
	
	/**
	 * notify all listeners that the one or more stack sizes have changed
	 */
	private void stacksChanged() {
		
		for ( CommandStackListener clist : listeners ) {
			clist.stacksUpdated(undo.size(), redo.size());
		}
	}
}
