package org.rzo.yajsw.os.posix;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileDescriptor;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.rzo.yajsw.io.CyclicBufferFileInputStream;
import org.rzo.yajsw.io.CyclicBufferFilePrintStream;
import org.rzo.yajsw.os.AbstractProcess;
import org.rzo.yajsw.os.OperatingSystem;
import org.rzo.yajsw.os.Process;
import org.rzo.yajsw.util.DaemonThreadFactory;

import com.sun.jna.Library;
import com.sun.jna.Memory;
import com.sun.jna.Native;
import com.sun.jna.NativeLibrary;
import com.sun.jna.Platform;
import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import com.sun.jna.ptr.IntByReference;

public class PosixProcess extends AbstractProcess
{
	protected int[]					_inPipe			= new int[2];
	protected int[]					_outPipe		= new int[2];
	protected int[]					_errPipe		= new int[2];
	public IntByReference			status			= new IntByReference();
	int								_exitCodeKill	= -1;

	protected static final Executor	executor		= Executors.newCachedThreadPool(new DaemonThreadFactory("posix.process.terminate"));
	protected boolean				lock			= true;
	volatile protected boolean		_terminated		= false;
	protected Utils					_utils			= new Utils();
	boolean							_stopWaiter		= false;
	String[]						_env			= null;
	int stdout = -1;// should not be called in all sub classes // getStdOutNo();
	int stderr = -1; //getStdErrNo();// CLibrary.INSTANCE.fileno(NativeLibrary.getInstance("c").getFunction(getStdErrName()).getPointer(0));
	int stdin = -1; // getStdInNo();// CLibrary.INSTANCE.fileno(NativeLibrary.getInstance("c").getFunction(getStdInName()).getPointer(0));


	public interface CLibrary extends Library
	{

		CLibrary	INSTANCE	= (CLibrary) Native.loadLibrary(Platform.isLinux() ? "libc.so.6" : "c", CLibrary.class);

		int fork();

		void exit(int status);

		String strerror(int errnum);

		/*
		 * int readlink (const char *filename, char *buffer, size_t size)
		 */
		short readlink(String filename, Memory buffer, short size);

		/*
		 * int execv (const charfilename, charconst argv[])
		 */
		int execvp(String filename, String[] argv);

		/*
		 * execve (const char *filename, char *const argv[], char *const env[])
		 */
		int execve(String path, String[] argv, String[] envp);

		/*
		 * int pipe (int filedes[2])
		 */
		int pipe(int filedes[]);

		/*
		 * int dup2(int oldfd, int newfd)
		 */
		int dup2(int oldfd, int newfd);

		/*
		 * int close(int fd)
		 */
		int close(Pointer fd);

		int close(int fd);

		/*
		 * mode_t umask (mode_t mask)
		 */
		void umask(int mask);

		int setsid();

		/*
		 * FILE freopen ( const char filename, const char mode, FILE stream );
		 */
		Pointer freopen(String filename, String mode, int stream);

		/*
		 * int kill (pid_t pid, int signum)
		 */
		int kill(int pid, int signum);

		static final int	SIGTERM	= 15;
		static final int	SIGKILL	= 9;

		/*
		 * pid_t waitpid(pid_t pid, intstat_loc, int options);
		 */
		int waitpid(int pid, IntByReference stat_loc, int options);

		static final int	ESRCH	= 3;

		/*
		 * int chdir(const charpath);
		 */
		int chdir(String path);

    Pointer getcwd( Memory buffer, short size );

    static final int WNOHANG   = 1; /* don't hang in wait */
    static final int WUNTRACED = 2; /*
                                     * tell about stopped, untraced children
                                     */

		/*
		 * int fputc (int c, FILEstream)
		 */
		int fputc(int c, Pointer stream);

		/*
		 * FILEfdopen(int fildes, const chartype);
		 */
		Pointer fdopen(Pointer fildes, String type);

		/*
		 * int fileno(FILEstream);
		 */
		int fileno(Pointer stream);

		/*
		 * struct dirent64 { __u64 d_ino; __s64 d_off; unsigned short d_reclen;
		 * unsigned char d_type; char d_name[256]; };
		 */
		class dirent64 extends Structure
		{
			public long		d_ino;
			public long		d_off;
			public short	d_reclen;
			public char		d_type;
			public char[]	d_name	= new char[256];

			public String getName()
			{
				return getPointer().getString(8 + 8 + 2 + 1, false);
			}
		};

		/*
		 * struct dirent { long d_ino; off_t d_off; unsigned short d_reclen;
		 * char d_name[NAME_MAX+1]; };
		 */
		class dirent extends Structure
		{
			public int		d_ino;
			public int		d_off;
			public short	d_reclen;
			public String	d_name;
		};

		/*
		 * DIR opendir (const chardirname)
		 */
		Pointer opendir(String dirname);

		/*
		 * struct dirent64 readdir64 (DIRdirstream)
		 */
		dirent64 readdir64(Pointer dirstream);

		/*
		 * int closedir (DIRdirstream)
		 */
		int closedir(Pointer dirstream);

		/*
		 * int nice (int increment)
		 */
		int nice(int increment);

		/*
		 * int sched_setaffinity (pid_t pid, size_t cpusetsize, const cpu_set_t
		 * cpuset)
		 */
		int sched_setaffinity(int pid, int cpusetsize, IntByReference cpuset);

		/*
		 * pid_t getpid(void);
		 */
		int getpid();

		/*
		 * int symlink (const charoldname, const charnewname)
		 */
		int symlink(String oldname, String newname);

		/*
		 * struct passwd
		 * 
		 * The passwd data structure is used to hold information about entries
		 * in the system user data base. It has at least the following members:
		 * 
		 * charpw_name The user's login name. charpw_passwd. The encrypted
		 * password string. uid_t pw_uid The user ID number. gid_t pw_gid The
		 * user's default group ID number. charpw_gecos A string typically
		 * containing the user's real name, and possibly other information such
		 * as a phone number. charpw_dir The user's home directory, or initial
		 * working directory. This might be a null pointer, in which case the
		 * interpretation is system-dependent. charpw_shell The user's default
		 * shell, or the initial program run when the user logs in. This might
		 * be a null pointer, indicating that the system default should be used.
		 */

		public static class passwd extends Structure
		{
			public passwd(Pointer p)
			{
				super();
				if (p != null)
				{
					this.useMemory(p);
					this.read();
				}
			}

			public String	pw_name;
			public String	pw_passwd;
			public int		pw_uid;
			public int		pw_gid;
			public String	pw_gecos;
			public String	pw_dir;
			public String	pw_shell;

			public String getName()
			{
				return pw_name;
			}

			public int getUid()
			{
				return pw_uid;
			}

			public int getGid()
			{
				return pw_gid;
			}
		}

		/*
		 * struct passwd getpwnam (const charname) This function returns a
		 * pointer to a statically-allocated structure containing information
		 * about the user whose user name is name. This structure may be
		 * overwritten on subsequent calls to getpwnam.
		 * 
		 * A null pointer return indicates there is no user named name.
		 */
		Pointer getpwnam(String name);

		/*
		 * uid_t geteuid (void)
		 * 
		 * The geteuid function returns the effective user ID of the process.
		 */
		int geteuid();

		/*
		 * struct passwd getpwuid (uid_t uid)
		 * 
		 * This function returns a pointer to a statically-allocated structure
		 * containing information about the user whose user ID is uid. This
		 * structure may be overwritten on subsequent calls to getpwuid.
		 * 
		 * A null pointer value indicates there is no user in the data base with
		 * user ID uid.
		 */

		Pointer getpwuid(int uid);

		/*
		 * int setreuid (uid_t ruid, uid_t euid)
		 */
		int setreuid(int ruid, int euid);

		/*
		 * struct group * getgrgid (gid_t gid) This function returns a pointer
		 * to a statically-allocated structure containing information about the
		 * group whose group ID is gid. This structure may be overwritten by
		 * subsequent calls to getgrgid. A null pointer indicates there is no
		 * group with ID gid.
		 */
		Pointer getgrgid(int gid);

		/*
		 * gid_t getegid (void) The getegid function returns the effective group
		 * ID of the process.
		 */
		int getegid();

		/*
		 * struct group The group structure is used to hold information about an
		 * entry in the system group database. It has at least the following
		 * members: char *gr_name - The name of the group. gid_t gr_gid - The
		 * group ID of the group. char **gr_mem - A vector of pointers to the
		 * names of users in the group. Each user name is a null-terminated
		 * string, and the vector itself is terminated by a null pointer.
		 */
		public static class group extends Structure
		{
			public group(Pointer p)
			{
				super();
				if (p != null)
				{
					this.useMemory(p);
					this.read();
				}
				// for (int i = 0; i<this.size(); i++)
				// System.out.println(i+" "+p.getByte(i));
			}

			public String	gr_name		= null;
			public String	gr_password	= null;
			public int		gr_gid		= 0;
			public Pointer	gr_mem		= null;

			public String getName()
			{
				return gr_name;
			}

			public int getGid()
			{
				return gr_gid;
			}

		}

		/*
		 * struct group * getgrnam (const char *name) This function returns a
		 * pointer to a statically-allocated structure containing information
		 * about the group whose group name is name. This structure may be
		 * overwritten by subsequent calls to getgrnam. A null pointer indicates
		 * there is no group named name.
		 */

		Pointer getgrnam(String name);

		/*
		 * int setregid (gid_t rgid, gid_t egid) This function sets the real
		 * group ID of the process to rgid and the effective group ID to egid.
		 * If rgid is -1, it means not to change the real group ID; likewise if
		 * egid is -1, it means not to change the effective group ID. The
		 * setregid function is provided for compatibility with 4.3 BSD Unix,
		 * which does not support file IDs. You can use this function to swap
		 * the effective and real group IDs of the process. (Privileged
		 * processes are not limited to this usage.) If file IDs are supported,
		 * you should use that feature instead of using this function. See
		 * Enable/Disable Setuid. The return values and error conditions for
		 * setregid are the same as those for setreuid.
		 */
		int setregid(int rgid, int egid);

		/*
		 * int chmod (const charfilename, mode_t mode)
		 */
		int chmod(String filename, int mode);

		public static final int	S_IFIFO		= 0010000;						// named
		// pipe
		// (fifo)
		public static final int	S_IFCHR		= 0020000;						// character
		// special
		public static final int	S_IFDIR		= 0040000;						// directory
		public static final int	S_IFBLK		= 0060000;						// block
		// special
		public static final int	S_IFREG		= 0100000;						// regular
		public static final int	S_IFLNK		= 0120000;						// symbolic
		// link
		public static final int	S_IFSOCK	= 0140000;						// socket
		public static final int	S_IFMT		= 0170000;						// file
		// mask
		// for
		// type
		// checks
		public static final int	S_ISUID		= 0004000;						// set
		// user
		// id
		// on
		// execution
		public static final int	S_ISGID		= 0002000;						// set
		// group
		// id
		// on
		// execution
		public static final int	S_ISVTX		= 0001000;						// save
		// swapped
		// text
		// even
		// after
		// use
		public static final int	S_IRUSR		= 0000400;						// read
		// permission,
		// owner
		public static final int	S_IWUSR		= 0000200;						// write
		// permission,
		// owner
		public static final int	S_IXUSR		= 0000100;						// execute/search
		// permission,
		// owner
		public static final int	S_IRGRP		= 0000040;						// read
		// permission,
		// group
		public static final int	S_IWGRP		= 0000020;						// write
		// permission,
		// group
		public static final int	S_IXGRP		= 0000010;						// execute/search
		// permission,
		// group
		public static final int	S_IROTH		= 0000004;						// read
		// permission,
		// other
		public static final int	S_IWOTH		= 0000002;						// write
		// permission,
		// other
		public static final int	S_IXOTH		= 0000001;						// execute
		// permission,
		// other

		public static final int	ALL_READ	= S_IRUSR | S_IRGRP | S_IROTH;
		public static final int	ALL_WRITE	= S_IWUSR | S_IWGRP | S_IWOTH;
		public static final int	S_IXUGO		= S_IXUSR | S_IXGRP | S_IXOTH;

		public static class stat64 extends Structure
		{
			public long	st_dev;
			public long	st_ino;
			public long	st_nlink;
			public int	st_mode;
			public int	st_uid;
			public int	st_gid;
			public long	st_rdev;
			public long	st_size;
			public long	st_blksize;
			public long	st_blocks;
			public long	st_atime;		// Time of last access (time_t)
			public long	st_atimensec;	// Time of last access (nanoseconds)
			public long	st_mtime;		// Last data modification time (time_t)
			public long	st_mtimensec;	// Last data modification time
			// (nanoseconds)
			public long	st_ctime;		// Time of last status change (time_t)
			public long	st_ctimensec;	// Time of last status change
			// (nanoseconds)
			public long	__unused4;
			public long	__unused5;
			public long	__unused6;

			public boolean isSocket()
			{
				return (st_mode & S_IFMT) == S_IFSOCK;
			}
		}

		public static class stat extends Structure
		{
			public long		st_dev;
			public short	__pad1;
			public int		st_ino;
			public int		st_mode;
			public int		st_nlink;
			public int		st_uid;
			public int		st_gid;
			public long		st_rdev;
			public short	__pad2;
			public int		st_size;
			public int		st_blksize;
			public int		st_blocks;
			public int		st_atime;		// Time of last access (time_t)
			public int		st_atimensec;	// Time of last access (nanoseconds)
			public int		st_mtime;		// Last data modification time
			// (time_t)
			public int		st_mtimensec;	// Last data modification time
			// (nanoseconds)
			public int		st_ctime;		// Time of last status change
			// (time_t)
			public int		st_ctimensec;	// Time of last status change
			// (nanoseconds)
			public int		__unused4;
			public int		__unused5;

			public boolean isSocket()
			{
				return (st_mode & S_IFMT) == S_IFSOCK;
			}

		}

		/*
		 * int fstat (int filedes, struct stat *buf)
		 */
		int fstat(int filedes, Pointer buf);

	}// CLibrary

	public void destroy()
	{
		if (_outputStream != null)
			try
			{
				_outputStream.close();
			}
			catch (IOException e)
			{
				e.printStackTrace();
			}
		if (_inputStream != null)
			try
			{
				_inputStream.close();
			}
			catch (IOException e)
			{
				e.printStackTrace();
			}
		if (_errorStream != null)
			try
			{
				_errorStream.close();
			}
			catch (IOException e)
			{
				e.printStackTrace();
			}

		CLibrary.INSTANCE.close(_inPipe[1]);
		CLibrary.INSTANCE.close(_outPipe[0]);
		CLibrary.INSTANCE.close(_errPipe[0]);
		CLibrary.INSTANCE.close(_inPipe[0]);
		CLibrary.INSTANCE.close(_outPipe[1]);
		CLibrary.INSTANCE.close(_errPipe[1]);
	}

	public Collection getChildren()
	{
		// TODO Auto-generated method stub
		return null;
	}

	public int getCurrentPageFaults()
	{
		// TODO Auto-generated method stub
		return 0;
	}

	public int getCurrentPhysicalMemory()
	{
		// TODO Auto-generated method stub
		return 0;
	}

	public int getCurrentVirtualMemory()
	{
		int result = -1;
		if (!isRunning())
			return result;

		String stat = _utils.readFile("/proc/" + _pid + "/stat");
		// System.out.println("status "+status);
		if (status != null)
			try
			{
				// vsize (23th)
				String sp = "(?:[^\\s]+[\\s]+){22}(\\d+).+";
				Pattern p = Pattern.compile(sp, Pattern.DOTALL);
				Matcher m = p.matcher(stat);
				m.find();
				// get threads
				result = Integer.parseInt(m.group(1).trim());
			}
			catch (Exception ex)
			{
				if (_logger != null)
					_logger.info("Error in getCurrentVirtualMemory() " + ex.getMessage());
			}

		return result;
	}

	public boolean isRunning()
	{
		if (_pid < 1)
			return false;
		return _exitCode < 0;
	}

	public int getExitCode()
	{
		if (_exitCodeKill >= 0)
			return _exitCodeKill;
		return _exitCode;

	}

	public boolean kill(int code)
	{
		/*
		 * bkowal
		 * Suppress extraneous output unless debug is enabled.
		 */
		if (_logger != null && _debug )
			_logger.info("killing " + _pid);
		int count = 0;
		while (_exitCode < 0 && count < 3)
		{
			count++;
			if (_logger != null)
				_logger.info("send kill sig");
			int r = CLibrary.INSTANCE.kill(_pid, CLibrary.SIGKILL);
			if (r == 0)
			{
				_exitCodeKill = code;
				return true;
			}
			else
			{
				if (_logger != null)
					_logger.fine("error calling kill: " + r);
			}
			if (_exitCode < 0)
				try
				{
					Thread.sleep(100);
				}
				catch (InterruptedException e)
				{
					e.printStackTrace();
					Thread.currentThread().interrupt();
				}
		}
		return false;
	}

	public boolean killTree(int code)
	{
		// TODO Auto-generated method stub
		return false;
	}

	public boolean start()
	{
		// log(">> env 1 " +_environment.size());

		if (_arrCmd == null && _cmd == null)
			return false;
		if (_arrCmd == null)
		{
			_arrCmd = _cmd.split(" ");
			/*
			 * bkowal
			 * Suppress extraneous output unless debug is enabled.
			 */
			if (_debug)
			{
				log("exec: " + _cmd);
			}
		}
		else
		{
			String cmd = "";
			for (String c : _arrCmd)
			{
				if (c != null)
					cmd += c + " ";
			}
			/*
			 * bkowal
			 * Suppress extraneous output unless debug is enabled.
			 */
			if (_debug)
			{
				log("exec:" + cmd);
			}
		}
		// 
		if (stdout == -1)
		{
		 stdout = getStdOutNo();
		 stderr = getStdErrNo();
		 stdin = getStdInNo();
		}


		if (_environment.size() > 0)
		{
			_env = new String[_environment.size()];
			int i = 0;
			for (String[] entry : _environment)
				_env[i++] = entry[0] + "=" + entry[1];
		}
		else
			_env = null;

		int pid = 0;
		_exitCode = -2;
		String title = _title == null ? "yajsw" : _title;
		_terminated = false;
		if (_visible)
			setCommand(String.format("xterm -hold -sb -T %1$s -e %2$s", title, getCommand()));

		// System.out.println("exec \n"+getCommand());
		// System.out.println("working dir\n"+getWorkingDir());

		if (_visible)
			_pipeStreams = false;

		// if (_pipeStreams)
		{
			CLibrary.INSTANCE.pipe(_inPipe);
			CLibrary.INSTANCE.pipe(_outPipe);
			CLibrary.INSTANCE.pipe(_errPipe);
			// System.out.println(_outPipe[0]+" "+_outPipe[1]);
		}

		String forkLogName = "forkLog" + System.currentTimeMillis() + ".log";

		// fork a child process
		if ((pid = CLibrary.INSTANCE.fork()) == 0)
		{
			/*
			 * bkowal
			 * Suppress extraneous output unless debug is enabled.
			 */
			if (_debug)
			{
				System.out.println("fork 0");
			}

			// closeDescriptors();

			// set working dir
			if (getWorkingDir() != null)
				if (CLibrary.INSTANCE.chdir(getWorkingDir()) != 0)
					log("could not set working dir");
			
			/*
			 * bkowal
			 * Suppress extraneous output unless debug is enabled.
			 */
			if (_debug)
			{
				System.out.println("fork 1");
			}

			// set priority
			if (_priority == PRIORITY_BELOW_NORMAL)
			{
				if (CLibrary.INSTANCE.nice(1) == -1)
					log("could not set priority ");
			}
			else if (_priority == PRIORITY_LOW)
			{
				if (CLibrary.INSTANCE.nice(2) == -1)
					log("could not set priority ");
			}
			else if (_priority == PRIORITY_ABOVE_NORMAL)
			{
				if (CLibrary.INSTANCE.nice(-1) == -1)
					log("could not set priority ");
			}
			else if (_priority == PRIORITY_HIGH)
			{
				if (CLibrary.INSTANCE.nice(-2) == -1)
					log("could not set priority ");
			}
			if (getUser() != null)
				switchUser(getUser(), getPassword());
			/*
			 * bkowal
			 * Suppress extraneous output unless debug is enabled.
			 */
			if (_debug)
			{
				System.out.println("fork 2");
			}

			// try
			// {
			// closeDescriptors(new int[]{
			// stdin, stdout, stderr, _inPipe[1], _inPipe[0],
			// _outPipe[0], _outPipe[1], _errPipe[0],
			// _errPipe[1]
			// });
			// }
			// catch (Throwable ex)
			// {
			// ex.printStackTrace();
			// }

			// pipe streams to OS pipes
			if (_pipeStreams)
			{
				CLibrary.INSTANCE.close(_inPipe[1]);
				moveDescriptor(_inPipe[0], stdin);
				CLibrary.INSTANCE.close(_outPipe[0]);
				moveDescriptor(_outPipe[1], stdout);
				CLibrary.INSTANCE.close(_errPipe[0]);
				moveDescriptor(_errPipe[1], stderr);
			}

			try
			{
				int res;

				// disconect from parent
				CLibrary.INSTANCE.umask(0);
				if (CLibrary.INSTANCE.setsid() < 0)
				{
					CLibrary.INSTANCE.exit(-1);
				}
				if (_env != null)
				{
					res = CLibrary.INSTANCE.execve(_arrCmd[0], _arrCmd, _env);
				}
				else
				{
					res = CLibrary.INSTANCE.execvp(_arrCmd[0], _arrCmd);
				}
				int err = Native.getLastError();
				log("error in execv: errno " + err + " " + CLibrary.INSTANCE.strerror(err));
				log("exec res " + res);

			}
			catch (Exception ex)
			{
				ex.printStackTrace();
			}
			lock = false;
			// CLibrary.INSTANCE.exit(-1);
		} // child code
		else if (pid > 0)
		{
			_pid = pid;
			try
			{
				Thread.sleep(500);
			}
			catch (InterruptedException e1)
			{
			}
			// or pipe streams to cyclic buffer files
			if (_teeName != null && _tmpPath != null)
			{
				// System.out.println("opening tee streams");
				File f = new File(_tmpPath);
				try
				{
					if (!f.exists())
						f.mkdir();
				}
				catch (Exception ex)
				{
					if (_logger != null)
						_logger.throwing(PosixProcess.class.getName(), "start", ex);
					Thread.currentThread().interrupt();
				}
				try
				{
					// System.out.println("opening tee streams out");
					_inputStream = new CyclicBufferFileInputStream(createRWfile(_tmpPath, "out_" + _teeName));
				}
				catch (Exception e)
				{
					e.printStackTrace();
				}
				try
				{
					// System.out.println("opening tee streams err");
					_errorStream = new CyclicBufferFileInputStream(createRWfile(_tmpPath, "err_" + _teeName));
				}
				catch (Exception e)
				{
					if (_logger != null)
						_logger.throwing(PosixProcess.class.getName(), "start", e);
				}
				try
				{
					// System.out.println("opening tee streams in");
					_outputStream = new CyclicBufferFilePrintStream(createRWfile(_tmpPath, "in_" + _teeName));
				}
				catch (Exception e)
				{
					if (_logger != null)
						_logger.throwing(PosixProcess.class.getName(), "start", e);
				}
				// System.out.println("- opening tee streams");
			}
			/*
			 * if (!_pipeStreams) {
			 * System.out.println("setting out streams to /dev/null/");
			 * CLibrary.INSTANCE.freopen("/dev/null", "w", _outPipe[0]);
			 * System.out.println("setting err streams to /dev/null/");
			 * CLibrary.INSTANCE.freopen("/dev/null", "w", _errPipe[0]);
			 * //System.out.println("setting streams to /dev/null/");
			 * //CLibrary.INSTANCE.freopen("/dev/null", "r", _inPipe[1]);
			 * System.out.println("- setting streams to /dev/null/"); }
			 */

			// System.out.println("parent");
			if (_pipeStreams && _teeName == null)
			{
				writefd(in_fd, _inPipe[1]);
				writefd(out_fd, _outPipe[0]);
				writefd(err_fd, _errPipe[0]);

				_outputStream = new BufferedOutputStream(new FileOutputStream(in_fd));
				_inputStream = new BufferedInputStream(new FileInputStream(out_fd));
				_errorStream = new BufferedInputStream(new FileInputStream(err_fd));

				CLibrary.INSTANCE.close(_inPipe[0]);
				CLibrary.INSTANCE.close(_outPipe[1]);
				CLibrary.INSTANCE.close(_errPipe[1]);

			}
			if (_cpuAffinity != AFFINITY_UNDEFINED)
			{
				IntByReference affinity = new IntByReference();
				affinity.setValue(_cpuAffinity);
				if (CLibrary.INSTANCE.sched_setaffinity(_pid, 4, affinity) == -1)
					log("error setting affinity");
			}
			_stopWaiter = true;
			executor.execute(new Runnable()
			{

        public void run()
        {
          int r = 0;
          while ( r != _pid && r != -1 )
          {
            r = CLibrary.INSTANCE.waitpid( _pid, status, 0 );
    		/*
    		 * bkowal
    		 * Suppress extraneous output unless debug is enabled.
    		 */
            if ( _logger != null && _debug )
              _logger.info( "waitpid " + r + " " + status.getValue() );
          }
          if ( r == _pid )
          {
            int code = status.getValue();

            // Exited Normally
            if ( WIFEXITED( code ) != 0 )
              _exitCode = WEXITSTATUS( code );
            // Exited Ab-Normally
            else
              _exitCode = 0;
          }
  		/*
  		 * bkowal
  		 * Suppress extraneous output unless debug is enabled.
  		 */
          if ( _logger != null && _debug )
            _logger.info( "exit code posix process: " +status.getValue()+" application: "+ _exitCode );
          _terminated = true;
        }

			});

			/*
			 * bkowal
			 * Suppress extraneous output unless debug is enabled.
			 */
			if (_logger != null && _debug)
				_logger.info("started process " + _pid);
			return true;
		} // parent process
		else if (pid < 0)
		{
			if (_logger != null)
				_logger.info("failed to fork: " + pid);
			return false;
		}
		return false;

	}

  public int WIFEXITED( int code ) {
    return (code & 0xFF);
  }

  public int WEXITSTATUS( int code ) {
    return ((code >> 8) & 0xFF);
  }

  protected File createRWfile( String path, String fname ) throws IOException
  {
    File result = new File( path, fname );
    result.deleteOnExit();
    /*
     * String absPath = result.getAbsolutePath(); System.out.println("PosixProcess.createRWfile "+absPath); if
     * (!result.exists()) { result.createNewFile(); } result.deleteOnExit(); String name = result.getCanonicalPath();
     * System.out.println("chmod 777 " + name); //Runtime.getRuntime().exec("chmod 777 " + name); int res =
     * CLibrary.INSTANCE.chmod(absPath, 777); if (res != 0) System.out.println("chmod failed "+res);
     */

		return result;
	}

	public boolean stop(int timeout, int code)
	{
		/*
		 * bkowal
		 * Suppress extraneous output unless debug is enabled.
		 */
		if (_logger != null && _debug )
			_logger.info("killing " + _pid);
		if (!isRunning())
			return true;
		int r = CLibrary.INSTANCE.kill(_pid, CLibrary.SIGTERM);
		waitFor(timeout);
		int count = 0;
		while (isRunning() && count++ < 4)
		{
			CLibrary.INSTANCE.kill(_pid, CLibrary.SIGKILL);
			if (isRunning())
				try
				{
					Thread.sleep(100);
				}
				catch (InterruptedException e)
				{
					if (_logger != null)
						_logger.throwing(PosixProcess.class.getName(), "stop", e);
					Thread.currentThread().interrupt();
				}
		}
		return !isRunning();
	}

	protected void moveDescriptor(int fd_from, int fd_to)
	{
		// System.out.println("move desc "+fd_from+" "+fd_to);
		if (fd_from != fd_to)
		{
			CLibrary.INSTANCE.dup2(fd_from, fd_to);
			CLibrary.INSTANCE.close(fd_from);
		}
	}

	int closeDescriptors(int[] avoid)
	{
		// Pointer dir;
		// CLibrary.dirent64 dirp;
		// int from_fd = FAIL_FILENO + 1;

		/*
		 * We're trying to close all file descriptors, but opendir() might
		 * itself be implemented using a file descriptor, and we certainly don't
		 * want to close that while it's in use. We assume that if opendir() is
		 * implemented using a file descriptor, then it uses the lowest numbered
		 * file descriptor, just like open(). So we close a couple explicitly.
		 */

		// close(from_fd); /* for possible use by opendir() */
		// close(from_fd + 1); /* another one for good luck */
		/*
		 * if ((dir = CLibrary.INSTANCE.opendir("/proc/self/fd")) == null) {
		 * //log("error in opendir(/proc/self/fd) "+dir); return 0; }
		 * 
		 * 
		 * /* We use readdir64 instead of readdir to work around Solaris bug
		 * 6395699: /proc/self/fd fails to report file descriptors >= 1024 on
		 * Solaris 9
		 */
		/*
		 * while ((dirp = CLibrary.INSTANCE.readdir64(dir)) != null) try {
		 * log("readdir64 dir "+dir); dirp.read(); String name = dirp.getName();
		 * if (name == null) return 0; if (name.contains(".")) continue;
		 * log("closing "+name); int fd = Integer.parseInt(name);
		 * log("closing "+fd); //int r = CLibrary.INSTANCE.close(fd);
		 * //log("closing "+name+" "+r); } catch (Exception ex){
		 * ex.printStackTrace(); }
		 * 
		 * CLibrary.INSTANCE.closedir(dir);
		 */
		/*
		 * File f = new File("/proc/self/fd"); String[] ff = f.list(); f = null;
		 * int start = 19; //if (start < 0) //start = 0; for (int j =
		 * ff.length-2; j>=0; j--) { String x = ff[j]; if (x == null ||
		 * "".equals(x)) continue; //CLibrary.stat buf = new CLibrary.stat();
		 * //buf.size();
		 * 
		 * short BUFSIZE = 512; Memory result = new Memory(BUFSIZE);
		 * result.clear(); String readLink = null; short size =
		 * CLibrary.INSTANCE.readlink("/proc/self/fd/"+x, result,
		 * (short)(BUFSIZE-1)); if (size <= 0) {
		 * System.out.println("error reading /proc/self/fd/"+x); } else {
		 * result.setByte((long)size, (byte)0);
		 * System.out.println(x+" -> "+result.getString(0)); readLink =
		 * result.getString(0); }
		 * 
		 * 
		 * try { int xx = Integer.parseInt(x); /* int res =
		 * CLibrary.INSTANCE.fstat(xx, buf.getPointer()); if (res == 0) {
		 * buf.read(); System.out.println("mode "+xx+" "+buf.st_mode); if
		 * (buf.isSocket()) System.out.println("is socket "+xx); } else
		 * System.out.println("error in fstat "+res+" "+xx);
		 */
		/*
		 * boolean remove = true; for (int i=0; i<avoid.length; i++) { if (xx ==
		 * avoid[i]) { remove = false; break; }
		 * 
		 * } //* //if (xx > 34) if (readLink != null && (!(xx < 2 ||
		 * readLink.contains("rt.jar") || readLink.contains("wrapper.jar") ||
		 * readLink.contains("jna-") || readLink.contains("jnacontrib") ))) //
		 * if (readLink != null && (readLink.startsWith("socket:["))) {
		 * System.out.println("closing "+xx); //CLibrary.INSTANCE.close(xx); }
		 * else System.out.println("not closing "+xx); //* } catch (Throwable
		 * ex) { ex.printStackTrace(); }
		 * 
		 * }
		 */
		for (int i = 10; i < 54; i++)
			CLibrary.INSTANCE.close(i);
		for (int i = 56; i < 76; i++)
			CLibrary.INSTANCE.close(i);

		return 1;
	}

	public void waitFor()
	{
		waitFor(Long.MAX_VALUE);
	}

	public void waitFor(long timeout)
	{
		long start = System.currentTimeMillis();
		File f = new File("/proc/" + _pid);

		while (System.currentTimeMillis() - start < timeout)
		{
			if (!isRunning() || !f.exists())
			{
				return;
			}
			try
			{
				Thread.sleep(100);
			}
			catch (InterruptedException e)
			{
				if (_logger != null)
					_logger.throwing(PosixProcess.class.getName(), "waitFor", e);
				Thread.currentThread().interrupt();
			}
		}

	}

	// test
	/**
	 * The main method.
	 * 
	 * @param args
	 *            the arguments
	 * @throws IOException
	 */
	public static void main(String[] args) throws IOException
	{
		PosixProcess[] p = new PosixProcess[1];
		boolean pipe = true;
		for (int i = 0; i < p.length; i++)
		{
			p[i] = new PosixProcess();

			// p[i].setPipeStreams(true, false);
			// p[i].setCommand("xeyes");// "java -cp yajsw.jar
			// org.rzo.yajsw.HelloWorld >
			// t.log");
			// p[i].setCommand("/usr/java/jre1.5.0_10/bin/java -classpath ./bin test.HelloWorld");
			p[i]
					.setCommand("/usr/java/jre1.5.0_10/bin/java -classpath /home/test/rzodyndns/test/wrapper.jar -Dwrapper.config=/home/test/rzodyndns/test/bat/../conf/wrapper.conf -Dwrapper.port=15003 -Dwrapper.key=6566092584194115879 -Dwrapper.teeName=6566092584194115879$1225016378236 -Dwrapper.tmpPath=/tmp org.rzo.yajsw.app.WrapperJVMMain");
			// p[i].setWorkingDir("/home/test/rzodyndns/test/bat/.");
			p[i].setVisible(false);
			// p[i].setPriority(PRIORITY_BELOW_NORMAL);
			// p[i].setCpuAffinity(1);

			p[i].setPipeStreams(pipe, false);
		}
		boolean doit = true;
		while (doit)
		{
			doit = false;
			// System.out.println("START");
			// doit = false;
			for (int i = 0; i < p.length; i++)
			{

				p[i].start();
				// p[i].getPid();
				// Runtime.getRuntime().exec(p[i].getCommand());
				// System.out.println("started");
				// for (int j=0; i<10000; j++)
				// {
				// System.out.println("b"+j);
				// try
				// {
				// Thread.sleep(00);
				// }
				// catch (InterruptedException e)
				// {
				// // TODO Auto-generated catch block
				// e.printStackTrace();
				// }
				// }
				// return;
				try
				{
					Thread.yield();
					Thread.sleep(1000);
				}
				catch (InterruptedException e1)
				{
					// TODO Auto-generated catch block
					e1.printStackTrace();
				}
				if (pipe)
				{
					InputStreamReader isr = new InputStreamReader(p[i].getInputStream());
					// System.out.println("in stream " + p[i].getInputStream() +
					// " " + p[i].getInputStream().available());

					BufferedReader br = new BufferedReader(isr);
					String line = "?";
					int k = 0;
					try
					{

						while (k < 10 && (line = br.readLine()) != null)
						{
							System.out.println(line);
							k++;
						}

					}
					catch (Exception e)
					{
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}
			}
			p[0].waitFor(1000);
			// System.out.println("exit code "+p[0].getExitCode());
			System.out.println("KILL");

			for (int i = 0; i < p.length; i++)
			{
				// System.out.println(p[i].isRunning());
				p[i].kill(999);
				System.out.println("exit code " + p[i].getExitCode());
				// System.out.println(p[i].isRunning());
				// p[i].destory();
			}
			try
			{
				Thread.sleep(1000);
			}
			catch (InterruptedException e)
			{
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}

		// p.setCommand("java -classpath z:\dev\yajsw\wrapper.jar org.rzo." )
	}

	/**
	 * Writefd.
	 * 
	 * @param fd
	 *            the fd
	 * @param pointer
	 *            the pointer
	 */
	protected void writefd(FileDescriptor fd, int pointer)
	{
		try
		{
			// Field[] fields = FileDescriptor.class.getDeclaredFields();
			// System.out.println("fields");
			// for (Field field : fields){
			// System.out.println(field.getName());
			// }
			// System.out.println("writefd");
			Field handleField = FileDescriptor.class.getDeclaredField("fd");
			handleField.setAccessible(true);
			Field peerField = Pointer.class.getDeclaredField("peer");
			peerField.setAccessible(true);
			long value = pointer;// peerField.getLong(pointer);
			// System.out.println(value);
			// System.out.flush();
			handleField.setInt(fd, (int) value);
			// System.out.println(fd.valid());
			// Method sync = FileDescriptor.class.getDeclaredMethod("sync", new
			// Class[0]);
			// sync.setAccessible(true);
			// sync.invoke(fd, new Object[0]);

		}
		catch (Exception e)
		{
			e.printStackTrace();
		}

	}

	public boolean reconnectStreams()
	{
		if (_teeName != null)
			try
			{
				_inputStream = new CyclicBufferFileInputStream(new File(_tmpPath, "out_" + _teeName));
				_errorStream = new CyclicBufferFileInputStream(new File(_tmpPath, "err_" + _teeName));
				_outputStream = new CyclicBufferFilePrintStream(new File(_tmpPath, "in_" + _teeName));
				return true;
			}
			catch (Exception ex)
			{
				ex.printStackTrace();
			}

		return false;
	}

	private String getCommandInternal()
	{
		String result = _utils.readFile("/proc/" + getPid() + "/cmdline");
		if (result == null || result.length() == 0)
			result = "?";
		// System.out.println("cmd line: "+result);
		return result;
	}

	private List<String[]> getEnvironmentInternal()
	{
		String result = _utils.readFile("/proc/" + getPid() + "/environ");
		return parseEnvironment(result);
	}

	private List<String[]> parseEnvironment(String env)
	{
		List<String[]> result = new ArrayList<String[]>();
		if (env == null || "".equals(env))
			return result;
		String sp = "(\\S+)=([^=.]+)( |$)";
		Pattern p = Pattern.compile(sp, Pattern.DOTALL);
		Matcher m = p.matcher(env);
		while (m.find())
		{
			String[] str = m.group().trim().split("=", 2);
			if (str.length == 2)
			{
				result.add(new String[]
				{ str[0], str[1] });
			}
		}
		return result;

	}

	protected String getWorkingDirInternal()
	{
		String result = null;
		File f = new File("/proc/" + getPid() + "/cwd");
		try
		{
			result = f.getCanonicalPath();
		}
		catch (IOException e)
		{
			e.printStackTrace();
		}
		return result;

	}

	public String getWorkingDir()
	{
		return _workingDir;
	}

	public static Process getProcess(int pid)
	{
		PosixProcess result = null;
		File f = new File("/proc/" + pid);
		if (f.exists())
		{
			// TODO this may not always work
			result = (PosixProcess) OperatingSystem.instance().processManagerInstance().createProcess();
			result._pid = pid;
			result._user = result.getUserInternal();
			result._cmd = result.getCommandInternal();
			result._workingDir = result.getWorkingDirInternal();
			result._environment = result.getEnvironmentInternal();

		}
		return result;
	}

	public static int currentProcessId()
	{
		return CLibrary.INSTANCE.getpid();
	}

	public String currentUser()
	{
		int euid = CLibrary.INSTANCE.geteuid();
		// log("current user euid "+ euid);
		Pointer p = CLibrary.INSTANCE.getpwuid(euid);
		if (p == null)
			log("could not get current user");
		return new CLibrary.passwd(p).getName();

	}

	public String currentGroup()
	{
		int egid = CLibrary.INSTANCE.getegid();
		// System.out.println("current group egid "+ egid);
		Pointer pg = CLibrary.INSTANCE.getgrgid(egid);
		if (pg == null)
		{
			log("could not get current group");
			return null;
		}
		return new CLibrary.group(pg).getName();
	}

	public String defaultGroup(String user)
	{
		Pointer p = CLibrary.INSTANCE.getpwnam(user);
		if (p == null)
		{
			log("could not get user " + user);
			return null;
		}
		int gid = new CLibrary.passwd(p).getGid();
		//System.out.println("default group gid " + gid);
		Pointer pg = CLibrary.INSTANCE.getgrgid(gid);
		if (pg == null)
		{
			log("could not get default group for user " + user);
			return null;
		}
		return new CLibrary.group(pg).getName();

	}

	public void switchUser(String name, String password)
	{
		if (name == null || "".equals(name))
			return;
		String[] x = name.split("\\\\");
		String user = x.length == 1 ? x[0] : x[1];
		String group = x.length == 1 ? null : x[0];

		if (group == null)
			group = defaultGroup(user);

		String currentUser = currentUser();
		String currentGroup = currentGroup();

		log("switch group " + currentGroup + " -> " + group);

		if (currentGroup != null && !currentGroup.equals(group))
		{
			Pointer p = CLibrary.INSTANCE.getgrnam(group);
			CLibrary.group g = new CLibrary.group(p);
			int newGid = g.getGid();
			String nam = g.getName();
			if (newGid == 0)
				log("could not get group " + group);
			// System.out.println("switching to group name/id "+nam+"/"+newGid);
			int res = CLibrary.INSTANCE.setregid(newGid, newGid);
			if (res != 0)
				log("could not change to group " + group);
		}

		log("switch user " + currentUser + " -> " + user);

		if (currentUser != null && !currentUser.equals(user))
		{
			Pointer p = CLibrary.INSTANCE.getpwnam(user);
			int newUid = new CLibrary.passwd(p).getUid();
			if (newUid == 0)
				log("could not get user " + user);
			int res = CLibrary.INSTANCE.setreuid(newUid, newUid);
			if (res != 0)
				log("could not change to user " + user);
		}

		currentUser = currentUser();
		if (!user.equals(currentUser))
			log("could not set user. current user: " + currentUser);

		currentGroup = currentGroup();
		if (!group.equals(currentGroup))
			log("could not set group. current group: " + currentGroup);

	}

	public String getUserInternal()
	{
		String status = _utils.readFile("/proc/" + _pid + "/status");
		// System.out.println("status "+status);
		if (status != null)
			try
			{
				// ruid, euid, suid fuid
				String sp = ".*[U|u]id:\\s*(\\d+)\\s*(\\d+)\\s*(\\d+)\\s*(\\d+).*";
				Pattern p = Pattern.compile(sp, Pattern.DOTALL);
				Matcher m = p.matcher(status);
				m.find();
				// get ruid
				int ruid = Integer.parseInt(m.group(1));
				//System.out.println("rudi " + ruid);
				Pointer po = CLibrary.INSTANCE.getpwuid(ruid);
				if (po == null)
					System.out.println("could not get user");
				return new CLibrary.passwd(po).getName().trim();
			}
			catch (Exception ex)
			{
				log("Error in getUser() " + ex.getMessage());
			}

		return "";

	}

	public String getUser()
	{
		return _user;
	}

	public String getStdInName()
	{
		return "stdin";
	}

	public String getStdOutName()
	{
		return "stdout";
	}

	public String getStdErrName()
	{
		return "stderr";
	}

	public int getStdOutNo()
	{
		return CLibrary.INSTANCE.fileno(NativeLibrary.getInstance("c").getFunction(getStdOutName()).getPointer(0));
	}

	public int getStdErrNo()
	{
		return CLibrary.INSTANCE.fileno(NativeLibrary.getInstance("c").getFunction(getStdErrName()).getPointer(0));
	}

	public int getStdInNo()
	{
		return CLibrary.INSTANCE.fileno(NativeLibrary.getInstance("c").getFunction(getStdInName()).getPointer(0));
	}

	public int getCurrentHandles()
	{
		if (!isRunning())
			return -1;
		File f = new File("/proc/" + _pid + "/fd");
		if (!f.exists() || !f.isDirectory())
			return -1;
		return f.list().length;
	}

	public int getCurrentThreads()
	{
		int result = -1;
		if (!isRunning())
			return result;
		String status = _utils.readFile("/proc/" + _pid + "/status");
		// System.out.println("status "+status);
		if (status != null)
			try
			{
				// thread count
				String sp = ".*[T|t]hreads:\\s*(\\d+).*";
				Pattern p = Pattern.compile(sp, Pattern.DOTALL);
				Matcher m = p.matcher(status);
				m.find();
				// get threads
				result = Integer.parseInt(m.group(1));
			}
			catch (Exception ex)
			{
				if (_logger != null)
					_logger.info("Error in getCurrentThreads() " + ex.getMessage());
			}

		return result;
	}

	long	_currentTotalCPU	= -1;
	long	_oldTotalCPU		= -1;
	long	_lastCPUReadTime	= Long.MAX_VALUE;

	public int getCurrentCpu()
	{
		int result = -1;
		if (!isRunning())
			return result;

		String stat = _utils.readFile("/proc/" + _pid + "/stat");
		// System.out.println("status "+status);
		if (status != null)
			try
			{
				// ucpu scpu (13th)
				String sp = "(?:[^\\s]+[\\s]+){13}(\\d+)\\s+(\\d+).+";
				Pattern p = Pattern.compile(sp, Pattern.DOTALL);
				Matcher m = p.matcher(stat);
				m.find();
				// get threads
				int ucpu = Integer.parseInt(m.group(1).trim());
				int scpu = Integer.parseInt(m.group(2).trim());
				// System.out.println(ucpu + "<<" + scpu);
				_oldTotalCPU = _currentTotalCPU;
				_currentTotalCPU = ucpu + scpu;
				double elapsed = ((double) (System.currentTimeMillis() - _lastCPUReadTime)) / 1000;
				double used = _currentTotalCPU - _oldTotalCPU;
				// System.out.println(elapsed + "<<" + used);
				if (elapsed > 0)
					result = (int) (used / elapsed);
				_lastCPUReadTime = System.currentTimeMillis();

			}
			catch (Exception ex)
			{
				if (_logger != null)
					_logger.info("Error in getCurrentCPU() " + ex.getMessage());
			}

		return result;
	}

	public boolean isTerminated()
	{
		return _terminated;
	}

	public boolean changeWorkingDir(String name)
	{
		File f = new File(name);
		String dir;
		if (!f.exists() || !f.isDirectory())
		{
			log("changeWorkingDirectory failed. file not found " + name);
			return false;
		}
		else
			try
			{
				dir = f.getCanonicalPath();
			}
			catch (IOException e)
			{
				if (_logger != null)
					_logger.throwing(PosixProcess.class.getName(), "setWorkingDirectory", e);
				return false;
			}
		boolean result = CLibrary.INSTANCE.chdir(name) == 0;
		if (result)
			System.setProperty("user.dir", dir);
		return result;
	}

	public void setTerminated(boolean terminated)
	{
		_terminated = terminated;
	}

	@Override
	public void setLogger(Logger logger)
	{
		super.setLogger(logger);
		_utils.setLog(logger);
	}

}
