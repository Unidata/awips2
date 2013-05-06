package org.rzo.yajsw.os.ms.win.w32;

import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.commons.lang.StringUtils;
import org.rzo.yajsw.os.ms.win.w32.Cluster.Clusapi.ClusterGroupState;
import org.rzo.yajsw.os.ms.win.w32.WindowsXPProcess.MyKernel32;

import com.sun.jna.Memory;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.WString;
import com.sun.jna.ptr.IntByReference;

public class Cluster
{
	static Logger	_log		= Logger.getLogger(Cluster.class.getCanonicalName());
	ExecutorService	threadPool	= Executors.newSingleThreadExecutor();

	public interface Clusapi extends com.sun.jna.win32.StdCallLibrary
	{
		Clusapi	INSTANCE	= (Clusapi) Native.loadLibrary("ClusApi", Clusapi.class);

		/*
		 * HCLUSTER WINAPI OpenCluster( __in_opt LPCWSTR lpszClusterName );
		 */
		Pointer OpenCluster(WString lpszClusterName);

		/*
		 * BOOL WINAPI CloseCluster( __in HCLUSTER hCluster );
		 */
		boolean CloseCluster(Pointer hCluster);

		/*
		 * HCHANGE WINAPI CreateClusterNotifyPort( __in HCHANGE hChange, __in
		 * HCLUSTER hCluster, __in DWORD dwFilter, __in DWORD_PTR dwNotifyKey );
		 */
		Pointer CreateClusterNotifyPort(Pointer hChange, Pointer hCluster, int dwFilter, IntByReference dwNotifyKey);

		final static int	CLUSTER_CHANGE_GROUP_STATE			= 0x00001000;
		final static int	CLUSTER_CHANGE_HANDLE_CLOSE			= 0x80000000;
		final static int	CLUSTER_CHANGE_CLUSTER_RECONNECT	= 0x00080000;
		final static int	CLUSTER_CHANGE_CLUSTER_STATE		= 0x20000000;
		final static int	CLUSTER_CHANGE_GROUP_DELETED		= 0x00002000;
		final static int	CLUSTER_CHANGE_GROUP_ADDED			= 0x00004000;

		// Results
		final static int	WAIT_TIMEOUT						= 0x102;
		final static int	ERROR_SUCCESS						= 0x0;
		final static int	ERROR_NO_MORE_ITEMS					= 0x103;

		// GetClusterGroupState results
		final static int	CLUSTER_GROUP_STATE_UNKNOWN			= -1;
		final static int	CLUSTER_GROUP_ONLINE				= 0;
		final static int	CLUSTER_GROUP_OFFLINE				= 1;
		final static int	CLUSTER_GROUP_FAILED				= 2;
		final static int	CLUSTER_GROUP_PARTIAL_ONLINE		= 3;
		final static int	CLUSTER_GROUP_PENDING				= 4;

		enum ClusterGroupState
		{
			Unknown, Online, Offline, Failed, PartialOnline, Pending;
			public static ClusterGroupState parse(int val)
			{
				switch (val)
				{
				case CLUSTER_GROUP_STATE_UNKNOWN:
					return Unknown;
				case CLUSTER_GROUP_ONLINE:
					return Online;
				case CLUSTER_GROUP_OFFLINE:
					return Offline;
				case CLUSTER_GROUP_FAILED:
					return Failed;
				case CLUSTER_GROUP_PARTIAL_ONLINE:
					return PartialOnline;
				case CLUSTER_GROUP_PENDING:
					return Pending;
				default:
					_log.severe("unknown cluster state: " + val);
					return Unknown;
				}
			}
		}

		/*
		 * DWORD WINAPI GetClusterNotify( __in HCHANGE hChange, __out DWORD_PTR
		 * *lpdwNotifyKey, __out LPDWORD lpdwFilterType, __out LPWSTR lpszName,
		 * __inout LPDWORD lpcchName, __in_opt DWORD dwMilliseconds );
		 */
		int GetClusterNotify(Pointer hChange, IntByReference lpdwNotifyKey, IntByReference lpdwFilterType, Memory lpszName, IntByReference lpcchName,
				int dwMilliseconds);

		/*
		 * BOOL WINAPI CloseClusterNotifyPort( __in HCHANGE hChange );
		 */
		boolean CloseClusterNotifyPort(Pointer hChange);

		/*
		 * HNODE WINAPI OpenClusterNode( __in HCLUSTER hCluster, __in LPCWSTR
		 * lpszNodeName );
		 */
		Pointer OpenClusterNode(Pointer hCluster, WString lpszNodeName);

		/*
		 * BOOL WINAPI CloseClusterNode( __in HNODE hNode );
		 */
		boolean CloseClusterNode(Pointer hNode);

		/*
		 * CLUSTER_NODE_STATE WINAPI GetClusterNodeState( __in HNODE hNode );
		 */
		int GetClusterNodeState(Pointer hNode);

		/*
		 * HCLUSENUM WINAPI ClusterOpenEnum( __in HCLUSTER hCluster, __in DWORD
		 * dwType );
		 */
		Pointer ClusterOpenEnum(Pointer hCluster, int dwType);

		Pointer ClusterNodeOpenEnum(Pointer hNode, int dwType);

		static int	CLUSTER_ENUM_NODE			= 1;
		static int	CLUSTER_ENUM_RESOURCE		= 4;
		static int	CLUSTER_ENUM_NETINTERFACE	= 32;
		static int	CLUSTER_ENUM_GROUP			= 8;

		/*
		 * DWORD WINAPI ClusterCloseEnum( __in HCLUSENUM hEnum );
		 */
		int ClusterCloseEnum(Pointer hEnum);

		/*
		 * DWORD WINAPI ClusterEnum( __in HCLUSENUM hEnum, __in DWORD dwIndex,
		 * __out LPDWORD lpdwType, __out LPWSTR lpszName, __inout LPDWORD
		 * lpcchName );
		 */
		int ClusterEnum(Pointer hEnum, int dwIndex, IntByReference lpdwType, Memory lpszName, IntByReference lpcchName);

		int ClusterNodeEnum(Pointer hEnum, int dwIndex, IntByReference lpdwType, Memory lpszName, IntByReference lpcchName);

		/*
		 * HRESOURCE WINAPI OpenClusterResource( __in HCLUSTER hCluster, __in
		 * LPCWSTR lpszResourceName );
		 */
		Pointer OpenClusterResource(Pointer hCluster, WString lpszResourceName);

		/*
		 * HGROUP WINAPI OpenClusterGroup( __in HCLUSTER hCluster, __in LPCWSTR
		 * lpszGroupName );
		 */
		Pointer OpenClusterGroup(Pointer hCluster, WString lpszGroupName);

		/*
		 * CLUSTER_GROUP_STATE WINAPI GetClusterGroupState( __in HGROUP hGroup,
		 * __out_opt LPWSTR lpszNodeName, __inout_opt LPDWORD lpcchNodeName );
		 */
		int GetClusterGroupState(Pointer hGroup, Memory lpszNodeName, IntByReference lpcchNodeName);
	}

	ArrayList<ClusterNodeChangeListener>	_listeners	= new ArrayList<ClusterNodeChangeListener>();
	boolean									_stopped	= true;

	public String getActiveNode()
	{
		String activeNode = null;

		try
		{
			Pointer cluster = Clusapi.INSTANCE.OpenCluster(null);
			Pointer hEnum = Clusapi.INSTANCE.ClusterOpenEnum(cluster, Clusapi.CLUSTER_ENUM_GROUP);
			int dwIndex = 0;
			IntByReference lpdwType = new IntByReference();
			IntByReference lpcchName = new IntByReference();
			Memory lpszName = new Memory(256);
			lpszName.clear();
			lpcchName.setValue(256);
			int result = 0;
			do
			{
				result = Clusapi.INSTANCE.ClusterEnum(hEnum, dwIndex, lpdwType, lpszName, lpcchName);
				if (result == Clusapi.ERROR_SUCCESS)
				{
					String group = lpszName.getString(0, true);
					ClusterGroupInfo info = getGroupNodeInfo(cluster, group);
					if (info != null)
						activeNode = info.getLocation();
				}
				dwIndex++;
			}
			while (result == 0);
		}
		catch (Exception ex)
		{
			_log.log(Level.SEVERE, "Error getting cluster information", ex);
		}
		return activeNode;
	}

	private ClusterGroupInfo getGroupNodeInfo(Pointer cluster, String groupName)
	{
		ClusterGroupInfo result = null;
		try
		{
			Pointer hGroup = Clusapi.INSTANCE.OpenClusterGroup(cluster, new WString(groupName));

			if (hGroup == null)
				throw new RuntimeException("Clusapi call to OpenClusterGroup returned err code " + MyKernel32.INSTANCE.GetLastError());

			IntByReference lpcchNodeName = new IntByReference();
			Memory lpszNodeName = new Memory(256);
			lpszNodeName.clear();
			lpcchNodeName.setValue(256);

			int state = Clusapi.INSTANCE.GetClusterGroupState(hGroup, lpszNodeName, lpcchNodeName);
			String location = lpszNodeName.getString(0, true);

			if (state == Clusapi.CLUSTER_GROUP_STATE_UNKNOWN)
				_log.severe("unknown group state for group " + groupName + " err code " + MyKernel32.INSTANCE.GetLastError());

			result = new ClusterGroupInfo(groupName, state, location);

			MyKernel32.INSTANCE.CloseHandle(hGroup);
		}
		catch (Exception e)
		{
			_log.log(Level.SEVERE, "Error while getting GroupActiveNode", e);
		}
		return result;
	}

	public Set<ClusterGroupInfo> getGroupInfo()
	{
		Pointer hCluster = Clusapi.INSTANCE.OpenCluster(null);
		if (hCluster == null)
			throw new RuntimeException("Clusapi call to OpenClusterGroup returned err code " + MyKernel32.INSTANCE.GetLastError());

		Pointer hEnum = Clusapi.INSTANCE.ClusterOpenEnum(hCluster, Clusapi.CLUSTER_ENUM_GROUP);
		if (hEnum == null)
			throw new RuntimeException("Clusapi call to ClusterOpenEnum returned err code " + MyKernel32.INSTANCE.GetLastError());

		Set<ClusterGroupInfo> result = new LinkedHashSet<ClusterGroupInfo>();

		try
		{
			IntByReference lpdwType = new IntByReference();
			IntByReference lpcchName = new IntByReference(0);
			Memory lpszName = new Memory(256);

			int dwIndex = 0;

			int returnValue = 0;
			do
			{
				lpdwType.setValue(0);
				lpcchName.setValue(0);
				lpszName.clear();
				lpcchName.setValue(256);

				returnValue = Clusapi.INSTANCE.ClusterEnum(hEnum, dwIndex, lpdwType, lpszName, lpcchName);

				if (returnValue == Clusapi.ERROR_SUCCESS)
				{
					String group = lpszName.getString(0, true);
					ClusterGroupInfo info = getGroupNodeInfo(hCluster, group);
					if (info != null)
						result.add(info);
				}

				if ((returnValue != Clusapi.ERROR_NO_MORE_ITEMS) && (returnValue != Clusapi.ERROR_SUCCESS))
					_log.log(Level.SEVERE, "strange returnValue from ClusApi" + returnValue);

				dwIndex++;
			}
			while (returnValue == 0);
		}
		catch (Exception e)
		{
			_log.log(Level.SEVERE, "Error while getting Cluster group information", e);
		}
		finally
		{
			MyKernel32.INSTANCE.CloseHandle(hEnum);
			MyKernel32.INSTANCE.CloseHandle(hCluster);
		}
		return result;
	}

	public void start()
	{
		Runnable check = null;
		synchronized (this)
		{
			if (_stopped)
			{
				check = new Runnable()
				{
					public void run()
					{
						IntByReference lpdwNotifyKey = new IntByReference();
						IntByReference lpdwFilterType = new IntByReference();
						IntByReference lpcchName = new IntByReference();
						IntByReference dwNotifyKey = new IntByReference();
						Memory lpszName = new Memory(256);
						Pointer minusOne = Pointer.createConstant(-1);
						int dwMilliseconds = 300 * 1000;
						final int dwFilter = Clusapi.CLUSTER_CHANGE_GROUP_STATE | Clusapi.CLUSTER_CHANGE_HANDLE_CLOSE
								| Clusapi.CLUSTER_CHANGE_GROUP_DELETED | Clusapi.CLUSTER_CHANGE_CLUSTER_STATE
								| Clusapi.CLUSTER_CHANGE_CLUSTER_RECONNECT | Clusapi.CLUSTER_CHANGE_GROUP_ADDED;

						while (!_stopped)
						{
							Pointer hCluster = null;
							Pointer hChange = null;

							long started = System.currentTimeMillis();

							try
							{
								lpdwNotifyKey.setValue(0);
								lpdwFilterType.setValue(0);
								lpcchName.setValue(0);
								dwNotifyKey.setValue(0);
								lpszName.clear();
								lpcchName.setValue(256);

								hCluster = Clusapi.INSTANCE.OpenCluster(null);
								if (hCluster == null)
									_log.severe("ClusApi.OpenCluster returned err code " + MyKernel32.INSTANCE.GetLastError());
								else
								{
									hChange = Clusapi.INSTANCE.CreateClusterNotifyPort(minusOne, hCluster, dwFilter, dwNotifyKey);
									if (hChange == null)
										_log.severe("ClusApi.CreateClusterNotifyPort returned err code " + MyKernel32.INSTANCE.GetLastError());
								}

								if (hCluster == null || hChange == null)
									Thread.sleep(5000);
								else
								{
									int result = Clusapi.INSTANCE.GetClusterNotify(hChange, lpdwNotifyKey, lpdwFilterType, lpszName, lpcchName,
											dwMilliseconds);

									if (result == Clusapi.ERROR_SUCCESS)
										doListeners(null, lpdwFilterType.getValue(), lpszName.getString(0, true));
									else if (result != Clusapi.WAIT_TIMEOUT) // 258
																				// =
																				// Wait
																				// Time
																				// Out
										_log.warning("ClusApi.GetClusterNotify result=" + result);
								}
							}
							catch (Throwable e)
							{
								_log.log(Level.SEVERE, "Error getting ClusterInformation", e);
							}
							finally
							{
								_log.info("check cluster took " + (System.currentTimeMillis() - started) + " ms");
								if (hChange != null)
								{
									try
									{
										Clusapi.INSTANCE.CloseClusterNotifyPort(hChange);
									}
									catch (Throwable e2)
									{
										e2.printStackTrace();
									}

									MyKernel32.INSTANCE.CloseHandle(hChange);
								}

								if (hCluster != null)
									MyKernel32.INSTANCE.CloseHandle(hCluster);
							}
						}
					}
				};
				_stopped = false;
			}
		}
		new Thread(check, "cluster listener thread").start();
	}

	private void doListeners(String activeNode, int lpdwFilterType, String lpszName)
	{
		// LOGGING
		try
		{
			switch (lpdwFilterType)
			{
			case Clusapi.CLUSTER_CHANGE_GROUP_ADDED:
				_log.severe("cluster group added: " + lpszName);
				break;

			case Clusapi.CLUSTER_CHANGE_GROUP_DELETED:
				_log.severe("cluster group deleted: " + lpszName);
				break;

			case Clusapi.CLUSTER_CHANGE_GROUP_STATE:
				_log.severe("cluster group state changed: " + lpszName);
				break;

			case Clusapi.CLUSTER_CHANGE_HANDLE_CLOSE:
				_log.severe("The queue receives a notification when a handle associated with a cluster object is closed. " + lpszName);
				break;

			case Clusapi.CLUSTER_CHANGE_CLUSTER_RECONNECT:
				_log.severe("The queue receives a notification when the connection to the cluster "
						+ "identified by hCluster is reestablished after a brief disconnect. Some events "
						+ "generated immediately before or after this event may have been lost. val=" + lpszName);
				break;

			case Clusapi.CLUSTER_CHANGE_CLUSTER_STATE:
				_log.severe("all attempts to communicate with the cluster failed, val=" + lpszName);
				break;

			default:
				_log.severe("unknown event id=" + Integer.toHexString(lpdwFilterType) + ", val=" + lpszName);
				break;
			}
		}
		catch (Throwable e)
		{
			_log.log(Level.SEVERE, "Error in Cluster Logging", e);
		}

		threadPool.execute(new Runnable()
		{
			public void run()
			{
				ArrayList<ClusterNodeChangeListener> listeners = new ArrayList<ClusterNodeChangeListener>();
				synchronized (_listeners)
				{
					listeners.addAll(_listeners);
				}

				for (ClusterNodeChangeListener l : listeners)
					try
					{
						l.nodeChanged();
					}
					catch (Throwable e)
					{
						_log.log(Level.SEVERE, "Error in ClusterNodeChangeListener.nodeChanged()", e);
					}
			}
		});
	}

	public void addNodeChangeListener(ClusterNodeChangeListener listener)
	{
		synchronized (_listeners)
		{
			_listeners.add(listener);
		}
	}

	public void stop()
	{
		_stopped = true;
	}

	public static void main(String[] args) throws UnknownHostException
	{
		final Cluster c = new Cluster();
		c.addNodeChangeListener(new ClusterNodeChangeListener()
		{
			public void nodeChanged()
			{
				try
				{
					System.out.println("new GroupInfo" + c.getGroupInfo());
				}
				catch (Exception e)
				{
					e.printStackTrace();
				}
			}
		});
		c.start();
		try
		{
			Thread.sleep(Integer.MAX_VALUE);
		}
		catch (InterruptedException e)
		{
			e.printStackTrace();
		}
	}

	public class ClusterGroupInfo
	{
		final private String	_groupName;
		final private String	_location;
		ClusterGroupState		_state	= ClusterGroupState.Unknown;

		public ClusterGroupInfo(String groupName, int state, String location)
		{
			_groupName = groupName;
			_location = location;
			_state = ClusterGroupState.parse(state);
		}

		public String getGroupName()
		{
			return _groupName;
		}

		public String getLocation()
		{
			return _location;
		}

		public ClusterGroupState getState()
		{
			return _state;
		}

		public boolean equals(ClusterGroupInfo info)
		{
			if (super.equals(info))
				return true;

			if (StringUtils.equals(_location, info._location) && StringUtils.equals(_groupName, info._groupName) && _state == info._state)
				return true;

			return false;
		}

		@Override
		public String toString()
		{
			return _groupName + "(" + _state + ", " + _location + ")";
		}
	}
}
