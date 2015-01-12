/**
 *   Adapted from jcgm class CGM; original copyright below
 */
/*
 * <copyright> Copyright 1997-2003 BBNT Solutions, LLC under sponsorship of the
 * Defense Advanced Research Projects Agency (DARPA).
 * Copyright 2009 Swiss AviationSoftware Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the Cougaar Open Source License as published by DARPA on
 * the Cougaar Open Source Website (www.cougaar.org).
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
package gov.noaa.nws.ncep.viz.rsc.ntrans.ncgm;

import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.BeginPicture;
import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.BeginPictureBody;
import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.CGM;
import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.ColourIndexPrecision;
import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.ColourModel;
import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.ColourPrecision;
import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.ColourSelectionMode;
import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.ColourValueExtent;
import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.Command;
import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.EdgeWidthSpecificationMode;
import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.EndMetafile;
import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.EndPicture;
import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.ICommandListener;
import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.IndexPrecision;
import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.IntegerPrecision;
import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.LineWidthSpecificationMode;
import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.MarkerSizeSpecificationMode;
import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.Message;
import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.Messages;
import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.RealPrecision;
import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.RestrictedTextType;
import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.VDCIntegerPrecision;
import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.VDCRealPrecision;
import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.VDCType;

import java.io.DataInput;
import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * The central JCGM class, extended/modified/stripped for NTRANS needs.
 * 
 * Represents one CGM image, as an ordered list of individual CGM commands.
 * 
 * @author bhebbard adapted from CGM.java by BBN
 * 
 */
public class NcCGM extends CGM implements Cloneable {

    // TODO static better? If so, change sister classes as well.
    private final Log logger = LogFactory.getLog(this.getClass());

    private List<Command> commands;

    private final List<ICommandListener> commandListeners = new ArrayList<ICommandListener>();

    private final static int INITIAL_NUM_COMMANDS = 500;

    private final static int MAX_COMMANDS_PER_IMAGE = 999999;

    public NcCGM() {
        // default constructor
    }

    public void read(DataInput in) throws IOException {
        reset();
        this.commands = new ArrayList<Command>(INITIAL_NUM_COMMANDS);
        int com = 0;
        Command c;
        do {
            if (com++ > MAX_COMMANDS_PER_IMAGE) {
                logger.error("Exceeded maxiumum CGM commands ("
                        + MAX_COMMANDS_PER_IMAGE
                        + ") for one image; adding EndPicture command automatically]");
                c = new EndPicture(0, 5, 0, in);
                break;
            }

            try {
                c = NcCommand.read(in);
            } catch (Exception e) {
                c = null;
                logger.error("Exception occurred interpreting CGM bytecode");
                e.printStackTrace();
            }

            if (c == null) {
                continue; // or should we add as null command?
            }

            if (c instanceof NcTextAlignment) {
                // TODO: special investigation --
                // NTRANS doesn't use this command quite as expected
                logger.debug("[CGM command #" + com + " completed]  "
                        + c.toString());
            }

            logger.debug("[CGM command #" + com + " completed]  "
                    + c.toString());

            for (ICommandListener listener : this.commandListeners) {
                listener.commandProcessed(c);
            }

            // get rid of all arguments after we read them
            c.cleanUpArguments();

            this.commands.add(c);

            if (c instanceof EndMetafile) {
                logger.warn("[Unexpected CGM EndMetafile command encountered]");
            }
            if (c instanceof EndPicture) {
                logger.debug("[CGM EndPicture command:  Picture is completed]");
            } else if (!(c instanceof INcCommand)) {
                if (!(c instanceof BeginPicture)
                        && !(c instanceof BeginPictureBody)) {
                    logger.warn("Unsupported CGM command encountered -- " + c
                            + "]");
                }
            }

        } while (!(c instanceof EndPicture));

    }

    /**
     * Adds the given listener to the list of command listeners
     * 
     * @param listener
     *            The listener to add
     */
    public void addCommandListener(ICommandListener listener) {
        this.commandListeners.add(listener);
    }

    /**
     * All the command classes with static data need to be reset here
     */
    private void reset() {
        ColourIndexPrecision.reset();
        ColourModel.reset();
        ColourPrecision.reset();
        ColourSelectionMode.reset();
        ColourValueExtent.reset();
        EdgeWidthSpecificationMode.reset();
        IndexPrecision.reset();
        IntegerPrecision.reset();
        LineWidthSpecificationMode.reset();
        MarkerSizeSpecificationMode.reset();
        RealPrecision.reset();
        RestrictedTextType.reset();
        VDCIntegerPrecision.reset();
        VDCRealPrecision.reset();
        VDCType.reset();

        Messages.getInstance().reset();
    }

    public List<Message> getMessages() {
        return Messages.getInstance();
    }

    public void showCGMCommands() {
        showCGMCommands(System.out);
    }

    public void showCGMCommands(PrintStream stream) {
        for (Command c : this.commands) {
            stream.println("Command: " + c);
        }
    }

    public List<Command> getCommands() {
        return Collections.unmodifiableList(this.commands);
    }

}
