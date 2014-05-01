package com.raytheon.uf.common.cache.disk;

import java.lang.ref.SoftReference;
import java.util.concurrent.atomic.AtomicInteger;

public class MetaData<K> {
	protected final String id;

	protected final Object syncObj;

	protected final String cacheFilePath;

	protected SoftReference<K> softRef = null;

	protected K ref = null;

	protected boolean modified = true;

	protected AtomicInteger numLockRequests = new AtomicInteger(0);

	protected MetaData(String cacheFilePath, K ref) {
		this.id = cacheFilePath;
		this.cacheFilePath = cacheFilePath;
		this.syncObj = new Object();
		this.ref = ref;
	}

	protected MetaData(String id, String cacheFilePath, K ref) {
		this.id = id;
		this.cacheFilePath = cacheFilePath;
		this.syncObj = new Object();
		this.ref = ref;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((cacheFilePath == null) ? 0 : cacheFilePath.hashCode());
		result = prime * result + ((id == null) ? 0 : id.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		MetaData other = (MetaData) obj;
		if (cacheFilePath == null) {
			if (other.cacheFilePath != null)
				return false;
		} else if (!cacheFilePath.equals(other.cacheFilePath))
			return false;
		if (id == null) {
			if (other.id != null)
				return false;
		} else if (!id.equals(other.id))
			return false;
		return true;
	}
}