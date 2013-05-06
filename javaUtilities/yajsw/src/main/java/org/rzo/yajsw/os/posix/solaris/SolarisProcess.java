package org.rzo.yajsw.os.posix.solaris;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.channels.FileChannel;

import org.rzo.yajsw.os.Process;
import org.rzo.yajsw.os.posix.PosixProcess;
import org.rzo.yajsw.os.posix.bsd.macosx.MacOsXProcess;

import com.sun.jna.Memory;

public class SolarisProcess extends PosixProcess
{

	/*
	 * #define PRARGSZ 80 /* number of chars of arguments typedef struct psinfo
	 * { 0int pr_flag; /* process flags (DEPRECATED; do not use) 4int pr_nlwp;
	 * /* number of active lwps in the process 8pid_t pr_pid; /* unique process
	 * id 12pid_t pr_ppid; /* process id of parent 16pid_t pr_pgid; /* pid of
	 * process group leader 20pid_t pr_sid; /* session id 24uid_t pr_uid; /*
	 * real user id 28uid_t pr_euid; /* effective user id 32gid_t pr_gid; /*
	 * real group id 36gid_t pr_egid; /* effective group id 40uintptr_t pr_addr;
	 * /* address of process 44size_t pr_size; /* size of process image in
	 * Kbytes 48size_t pr_rssize; /* resident set size in Kbytes 52size_t
	 * pr_pad1; 56dev_t pr_ttydev; /* controlling tty device (or PRNODEV) /* The
	 * following percent numbers are 16-bit binary /* fractions [0 .. 1] with
	 * the binary point to the /* right of the high-order bit (1.0 == 0x8000)
	 * 60ushort_t pr_pctcpu; /* % of recent cpu time used by all lwps 62ushort_t
	 * pr_pctmem; /* % of system memory used by process 62timestruc_t pr_start;
	 * /* process start time, from the epoch 94timestruc_t pr_time; /* usr+sys
	 * cpu time for this process 126timestruc_t pr_ctime; /* usr+sys cpu time
	 * for reaped children 158char pr_fname[PRFNSZ]; /* name of execed file char
	 * pr_psargs[PRARGSZ]; /* initial characters of arg list int pr_wstat; /* if
	 * zombie, the wait() status int pr_argc; /* initial argument count
	 * uintptr_t pr_argv; /* address of initial argument vector uintptr_t
	 * pr_envp; /* address of initial environment vector char pr_dmodel; /* data
	 * model of the process char pr_pad2[3]; taskid_t pr_taskid; /* task id
	 * projid_t pr_projid; /* project id int pr_nzomb; /* number of zombie lwps
	 * in the process poolid_t pr_poolid; /* pool id zoneid_t pr_zoneid; /* zone
	 * id id_t pr_contract; /* process contract int pr_filler[1]; /* reserved
	 * for future use lwpsinfo_t pr_lwp; /* information for representative lwp }
	 * psinfo_t;
	 */

	static int	PRARGSZ	= 80;	/* number of chars of arguments */

	public static class psinfo
	{
		ByteBuffer	_b;

		psinfo(ByteBuffer b)
		{
			_b = b;
		}

		int getPid()
		{
			return _b.getInt(8);
		}

		int getNlwp()
		{
			return _b.getInt(4);
		}

		int getPctcpu()
		{
			return (int) (((double) _b.getShort(60)) * 100) / 0x8000;
		}

		int getPr_size()
		{
			return _b.getInt(44);
		}

	}

	public int getStdOutNo()
	{
		return 1;
	}

	public int getStdErrNo()
	{
		return 2;
	}

	public int getStdInNo()
	{
		return 3;
	}
	

	public static Process getProcess(int pid)
	{
		SolarisProcess result = null;
		File f = new File("/proc/" + pid);
		if (f.exists())
		{
			result = new SolarisProcess();
			result._pid = pid;
			result._user = result.getUserInternal();
			result._cmd = result.getCommandInternal();
			result._workingDir = result.getWorkingDirInternal();
		}
		return result;
	}

	private String getCommandInternal()
	{
		String result = _utils.osCommand(String.format("pargs -l %1$s", _pid), 5000);
		if (result == null)
			result = "?";
		// System.out.println("cmd line: "+result);
		return result;
	}

	public String getUserInternal()
	{
		if (_pid < 0)
			return null;
		String cmd = String.format("ps -p %1$s -o user", _pid);
		String res = _utils.osCommand(cmd, 5000);
		if (res == null)
			return null;
		String[] resx = res.split(System.getProperty("line.separator"));
		return resx[1];
	}

	private psinfo getPsinfo()
	{
		FileChannel in;
		try
		{
			in = new FileInputStream(String.format("/proc/%1$s/psinfo", _pid)).getChannel();
		}
		catch (FileNotFoundException e)
		{
			System.out.println("error in getCurrentThreads() " + e.getMessage());
			return null;
		}
		int size;
		try
		{
			size = (int) in.size();
		}
		catch (IOException e)
		{
			System.out.println("error in getCurrentThreads() " + e.getMessage());
			return null;
		}
		ByteBuffer b = ByteBuffer.allocateDirect(size);
		b.order(ByteOrder.LITTLE_ENDIAN);
		try
		{
			in.read(b);
		}
		catch (IOException e)
		{
			System.out.println("error in getCurrent*() " + e.getMessage());
			return null;
		}
		psinfo s = new psinfo(b);
		try
		{
			in.close();
		}
		catch (IOException e)
		{
			e.printStackTrace();
		}
		return s;
	}

	public int getCurrentCpu()
	{
		int result = -1;
		if (!isRunning())
			return result;
		psinfo s = getPsinfo();
		if (s == null)
			return -1;
		return s.getPctcpu();
	}

	public int getCurrentThreads()
	{
		int result = -1;
		if (!isRunning())
			return result;
		psinfo s = getPsinfo();
		if (s == null)
			return -1;
		return s.getNlwp();
	}

	public int getCurrentVirtualMemory()
	{
		int result = -1;
		if (!isRunning())
			return result;
		psinfo s = getPsinfo();
		if (s == null)
			return -1;
		return s.getPr_size() * 1024;
	}

	protected String getWorkingDirInternal()
	{
    String result = "";
    String cwd;
    boolean success = false;
		String procCwd = "/proc/" + getPid() + "/cwd";
    try
    {

  	 short BUFSIZE = 4096;
		 Memory dir = new Memory(BUFSIZE);
		 dir.clear();

		 if( CLibrary.INSTANCE.getcwd(dir, BUFSIZE) != null )
     {
       cwd = dir.getString(0);
       dir.clear();

       if( CLibrary.INSTANCE.chdir(procCwd) == 0 )
       {
          if( CLibrary.INSTANCE.getcwd(dir, BUFSIZE) != null ){
            result = new File(dir.getString(0)).getCanonicalPath();
            success = true;
          }
          // Restore starting directory ( if different )
          CLibrary.INSTANCE.chdir(cwd);
       }

     }

     if( !success )
       System.out.println("error reading process working dir -> please edit wrapper.working.dir in configuration file");

    } catch (IOException e){  }

    return result;

//-KBG:		short BUFSIZE = 512;
//-KBG:		Memory result = new Memory(BUFSIZE);
//-KBG:		result.clear();
//-KBG:		short size = CLibrary.INSTANCE.readlink(f, result, (short) (BUFSIZE - 1));
//-KBG:		if (size <= 0)
//-KBG:		{
//-KBG:			System.out.println("error reading process working dir -> please edit wrapper.working.dir in configuration file");
//-KBG:			return f;
//-KBG:		}
//-KBG:		result.setByte((long) size, (byte) 0);
//-KBG:		return result.getString(0);

		/*
		 * String result = null; File f = new File("/proc/" + getPid() +
		 * "/cwd"); try { result = f.getCanonicalPath(); } catch (IOException e)
		 * { e.printStackTrace(); } return result;
		 */

	}

}
