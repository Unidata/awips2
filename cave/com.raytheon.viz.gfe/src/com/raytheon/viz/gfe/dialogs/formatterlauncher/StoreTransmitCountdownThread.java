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
package com.raytheon.viz.gfe.dialogs.formatterlauncher;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.ProgressBar;

import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.gfe.core.DataManager;

/**
 * Thread used for counting down Storing/Transmitting products.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 18 APR 2008  ###        lvenable    Initial creation
 * 20 AUG 2010  4687	   cjeanbap	   &quot;null&quot; showed up in
 *									   countdown message.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class StoreTransmitCountdownThread extends Thread {
	/**
	 * Parent display.
	 */
	private Display display;

	/**
	 * Progress bar to be updated.
	 */
	private ProgressBar progressBar;

	/**
	 * Flag indicating if the thread is done running.
	 */
	private boolean isDone = false;

	/**
	 * Flag indicating if the thread has been canceled.
	 */
	private boolean isCancelled = false;

	/**
	 * Count down label.
	 */
	private Label countdownLbl;

	/**
	 * Count down string.
	 */
	private String countdownStr;

	/**
	 * Counter.
	 */
	private int counter = 5;

	/**
	 * Store/Transmit callback.
	 */
	private IStoreTransmitProduct storeCB;

	/**
	 * Count down prefix string.
	 */
	private String countdownPrefix;

	/**
	 * Constructor.
	 * 
	 * @param display
	 *            Parent display.
	 * @param progressBar
	 *            Progress bar.
	 * @param countdownLbl
	 *            Count down label.
	 * @param countdownStr
	 *            Count down string.
	 * @param cb
	 *            Callback interface.
	 * @param isStore
	 *            True to display store, false to display transmit.
	 */
	public StoreTransmitCountdownThread(Display display,
			ProgressBar progressBar, Label countdownLbl, String countdownStr,
			IStoreTransmitProduct cb, boolean isStore) {
		this.display = display;
		this.progressBar = progressBar;
		this.countdownLbl = countdownLbl;
		this.countdownStr = countdownStr;
		this.storeCB = cb;
		countdownPrefix = new String();
		CAVEMode opMode = DataManager.getCurrentInstance().getOpMode();
		if (!opMode.equals(CAVEMode.OPERATIONAL)) {
			countdownPrefix = "Simulated ";
		}
		if (isStore == true) {
			countdownPrefix += "Store in ";
		} else {
			countdownPrefix += "Transmit in ";
		}
	}

	/**
	 * Thread's run method.
	 */
	@Override
	public void run() {
		isDone = false;
		countdownLabelStart();

		for (int i = 0; i < 6; i++) {
			try {
				Thread.sleep(1000);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}

			if (isCancelled == false) {
				display.asyncExec(new Runnable() {
					public void run() {
						if (progressBar.isDisposed()) {
							return;
						}
						// Increment the progress bar
						progressBar
								.setSelection(progressBar.getSelection() + 1);
						countdownLbl.setText(countdownPrefix + counter
								+ " seconds...");
						--counter;
					}
				});
			} else {
				break;
			}
		}

		if (isCancelled == false) {
			countdownLabelFinished();
		}

		isDone = true;

		storeCB.storeTransmitProduct();
	}

	/**
	 * Check if the thread is done running.
	 * 
	 * @return True if the thread is done running, false if it is still running.
	 */
	public boolean isDone() {
		return isDone;
	}

	/**
	 * Cancel the running thread.
	 */
	public void cancelThread() {
		isCancelled = true;
	}

	/**
	 * Check if the thread has been canceled.
	 * 
	 * @return True if the thread was canceled, false otherwise.
	 */
	public boolean threadCancelled() {
		return isCancelled;
	}

	/**
	 * Set the count down label to have a red background and white text while
	 * the Store/Transmit is in count down mode.
	 */
	private void countdownLabelStart() {
		display.asyncExec(new Runnable() {
			public void run() {
				countdownLbl.setBackground(display
						.getSystemColor(SWT.COLOR_RED));
				countdownLbl.setForeground(display
						.getSystemColor(SWT.COLOR_WHITE));
			}
		});
	}

	/**
	 * Set the count down label back to its original state.
	 */
	private void countdownLabelFinished() {
		display.asyncExec(new Runnable() {
			public void run() {
				countdownLbl.setText(countdownStr);
				countdownLbl.setBackground(display
						.getSystemColor(SWT.COLOR_WIDGET_BACKGROUND));
				countdownLbl.setForeground(display
						.getSystemColor(SWT.COLOR_BLACK));
			}
		});
	}
}
