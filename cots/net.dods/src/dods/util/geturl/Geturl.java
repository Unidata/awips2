/////////////////////////////////////////////////////////////////////////////
// Copyright (c) 1998, California Institute of Technology.  
// ALL RIGHTS RESERVED.   U.S. Government Sponsorship acknowledged. 
//
// Please read the full copyright notice in the file COPYRIGHT
// in this directory.
//
// Author: Jake Hamby, NASA/Jet Propulsion Laboratory
//         Jake.Hamby@jpl.nasa.gov
/////////////////////////////////////////////////////////////////////////////

package dods.util.geturl;
import dods.dap.*;
import dods.util.geturl.gui.StatusWindow;
import dods.util.Getopts;
import dods.util.InvalidSwitch;
import dods.util.OptSwitch;

/** Java port of the geturl command-line client. */
public class Geturl {

  /** Version of the client. */
  private static final String VERSION = "0.1 Beta";

  private static void usage() {
    System.err.println("Usage: geturl " +
	 "[dDagVvk] [c <expr>] [t <codes>] [m <num>] <url> [<url> ...]");
    System.err.println("[gVvk] [t <codes>] <file> [<file> ...]");
    System.err.println("In the first form of the command, dereference the URL");
    System.err.println("perform the requested operations. In the second, assume");
    System.err.println("the files are DODS data objects (stored in files or read");
    System.err.println("from pipes) and process them as if -D were given.");
    System.err.println("        d: For each URL, get the DODS DDS.");
    System.err.println("        a: For each URL, get the DODS DAS.");
    System.err.println("        D: For each URL, get the DODS Data.");
    System.err.println("        g: Show the progress GUI.");
    System.err.println("        v: Verbose.");
    System.err.println("        V: Version.");
    System.err.println("        c: <expr> is a contraint expression. Used with -D.");
    System.err.println("           NB: You can use a `?' for the CE also.");
    System.err.println("        m: Request the same URL <num> times.");
    System.err.println("        z: Don't ask the server to compress data.");
    System.err.println("        p: Dump the DODS Data as binary, not text.");
    System.err.println("       Without D, d or a, print the URL.");
  }

  public static void main(String args[]) {
    boolean get_das = false;
    boolean get_dds = false;
    boolean get_data = false;
    boolean dump_data = false;
    boolean gui = false;
    boolean cexpr = false;
    boolean verbose = false;
    boolean multi = false;
    boolean accept_deflate = true;
    int times = 1;
    String expr = "";
    
    try {
      Getopts opts = new Getopts("daDgVvc:m:zp", args);
      if(opts.getSwitch(new Character('d')).set)
	get_dds = true;
      if(opts.getSwitch(new Character('a')).set)
	get_das = true;
      if(opts.getSwitch(new Character('D')).set)
	get_data = true;
      if(opts.getSwitch(new Character('p')).set)
	dump_data = get_data = true;
      if(opts.getSwitch(new Character('V')).set) {
	System.err.println("geturl version: " + VERSION);
	System.exit(0);
      }
      if(opts.getSwitch(new Character('v')).set)
	verbose = true;
      if(opts.getSwitch(new Character('g')).set)
	gui = true;

      String optVal;
      optVal=opts.getSwitch(new Character('c')).val;
      if (optVal != null) {
	cexpr = true;
	expr = optVal;
      }

      optVal = opts.getSwitch(new Character('m')).val;
      if (optVal != null) {
	multi = true;
	times = Integer.parseInt(optVal);
      }

      if(opts.getSwitch(new Character('z')).set)
	accept_deflate = false;

      // If after processing all the command line options there is nothing left
      // (no URL or file) assume that we should read from stdin.
      String argp[] = opts.argList();
      if(argp.length == 0) {
	if (verbose)
	  System.err.println("Assuming standard input is a DODS data stream.");

	DConnect url = new DConnect(System.in);
	try {
	  StatusUI ui = null;
	  if (gui)
	    ui = new StatusWindow("stdin");
	  DataDDS dds = url.getData(ui);
	  processData(url, dds, verbose, dump_data, accept_deflate);
	}
	catch (DODSException e) {
	  System.err.println(e);
	  System.exit(1);
	}
	catch (Exception e) {
	  System.err.println(e);
	  e.printStackTrace();
	  System.exit(1);
	}
      }

      for(int i=0; i < argp.length; i++) {
	String nextURL = argp[i];
	if (verbose)
	  System.err.println("Fetching: " + nextURL);
	DConnect url = null;
	try {
	  url = new DConnect(nextURL, accept_deflate);
	}
	catch (java.io.FileNotFoundException e) {
	  System.err.println(nextURL + " is neither a valid URL nor a filename.");
	  System.exit(1);
	}

	if (url.isLocal()) {
	  if (verbose)
	    System.err.println("Assuming that the argument " + nextURL +
			       " is a file\n" +
			       "that contains a DODS data object; decoding.");

	  try {
	    StatusUI ui = null;
	    if (gui)
	      ui = new StatusWindow(nextURL);
	    DataDDS dds = url.getData(ui);
	    processData(url, dds, verbose, dump_data, accept_deflate);
	  }
	  catch (DODSException e) {
	    System.err.println(e);
	    System.exit(1);
	  }
	  catch (Exception e) {
	    System.err.println(e);
	    e.printStackTrace();
	    System.exit(1);
	  }
	}

	if (get_das) {
	  for (int j=0; j<times; j++) {
	    try {
	      DAS das = url.getDAS();
	      if (verbose) {
		System.err.println("Server version: " + url.getServerVersion());
		System.err.println("DAS:");
	      }
	      das.print(System.out);
	    }
	    catch (DODSException e) {
	      System.err.println(e);
	      e.printStackTrace();
	      System.exit(1);
	    }
	    catch (java.io.FileNotFoundException e) {
	      System.err.println(e);
	      System.exit(1);
	    }
	    catch (Exception e) {
	      System.err.println(e);
	      e.printStackTrace();
	      System.exit(1);
	    }
	  }
	}

	if (get_dds) {
	  for (int j=0; j<times; j++) {
	    try {
	      DDS dds = url.getDDS();
	      if (verbose) {
		System.err.println("Server version: " + url.getServerVersion());
		System.err.println("DDS:");
	      }
	      dds.print(System.out);
	    }
	    catch (DODSException e) {
	      System.err.println(e);
	      System.exit(1);
	    }
	    catch (java.io.FileNotFoundException e) {
	      System.err.println(e);
	      System.exit(1);
	    }
	    catch (Exception e) {
	      System.err.println(e);
	      e.printStackTrace();
	      System.exit(1);
	    }
	  }
	}

	if (get_data) {
	  if ((cexpr==false) && (nextURL.indexOf('?') == -1)) {
	    System.err.println("Must supply a constraint expression with -D.");
	    continue;
	  }
	  for (int j=0; j<times; j++) {
	    try {
	      StatusUI ui = null;
	      if (gui)
		ui = new StatusWindow(nextURL);
	      DataDDS dds = url.getData(expr, ui);
	      processData(url, dds, verbose, dump_data, accept_deflate);
	    }
	    catch (DODSException e) {
	      System.err.println(e);
	      System.exit(1);
	    }
	    catch (java.io.FileNotFoundException e) {
	      System.err.println(e);
	      System.exit(1);
	    }
	    catch (Exception e) {
	      System.err.println(e);
	      e.printStackTrace();
	      System.exit(1);
	    }
	  }
	}
      }
    }
    catch (InvalidSwitch e) {
      usage();
      System.exit(1);
    }
    // we must explicitly exit in case we opened a StatusWindow which might
    // otherwise prevent the Java VM from quitting
    System.exit(0);
  }

  private static void processData(DConnect url, DataDDS dds, boolean verbose,
				  boolean dump_data, boolean compress) {
    if (verbose)
      System.err.println("Server version: " + url.getServerVersion());
    
    if (dump_data) {
      try {
	dds.externalize(System.out, compress, true);
      }
      catch (java.io.IOException e) {
	System.err.println(e);
      }
    } else {
      System.out.println("The data:");
      dds.printVal(System.out);
      System.out.println();
    }
  }
}
