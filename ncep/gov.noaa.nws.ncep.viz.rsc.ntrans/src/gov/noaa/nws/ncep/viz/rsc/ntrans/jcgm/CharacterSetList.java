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
package gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm;

import java.io.*;
import java.util.HashMap;
import java.util.Map;


/**
 * Class=1, Element=14
 * @author xphc (Philippe Cadé)
 * @author BBNT Solutions
 * @version $Id$
 */
public class CharacterSetList extends Command {
	enum Type {
		_94_CHAR_G_SET,
		_96_CHAR_G_SET,
		_94_CHAR_MBYTE_G_SET,
		_96_CHAR_MBYTE_G_SET,
		COMPLETE_CODE
	}
	
	private Map<Type, String> characterSets;
	
	public CharacterSetList(int ec, int eid, int l, DataInput in)
            throws IOException {
        super(ec, eid, l, in);
        
        this.characterSets = new HashMap<Type, String>();
        
        while (this.currentArg < this.args.length) {
        	int typ = makeEnum();
        	Type type;
        	switch (typ) {
        	case 0: // 94 character G set
        		type = Type._94_CHAR_G_SET;
        		break;
        	case 1: // 96 character G set
        		type = Type._96_CHAR_G_SET;
        		break;
        	case 2: // 94 character multibyte G set
        		type = Type._94_CHAR_MBYTE_G_SET;
        		break;
        	case 3: // 96 character multibyte G set
        		type = Type._96_CHAR_MBYTE_G_SET;
        		break;
        	case 4:
        		type = Type.COMPLETE_CODE;
        		break;
        	default:
        		// XXX: which default to use?
        		type = Type.COMPLETE_CODE;
        		unsupported("unsupported character set type "+typ);
        	}

        	String characterSetDesignation = makeFixedString();
        	this.characterSets.put(type, characterSetDesignation);
        	
        	if (characterSetDesignation.length() > 2) {
        		int c = characterSetDesignation.charAt(0);
        		if (c == 27) {
        			// 27 == ESC
        			c = characterSetDesignation.charAt(1);
        			if (c == 22) {
        				int revNumber = characterSetDesignation.charAt(2);
        			}
        		}
        	}
        }
        
        unimplemented("CharacterSetList");
        
        // make sure all the arguments were read
        assert (this.currentArg == this.args.length);
    }

    @Override
	public String toString() {
    	StringBuilder sb = new StringBuilder();
    	sb.append("CharacterSetList ");
    	
    	for (Type type: this.characterSets.keySet()) {
    		sb.append("[").append(type).append(",").append(this.characterSets.get(type)).append("]");
    	}
    	
        return sb.toString();
    }
}

/*
 * vim:encoding=utf8
 */
