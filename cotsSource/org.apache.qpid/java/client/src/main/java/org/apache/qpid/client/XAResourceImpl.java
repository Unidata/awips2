/* Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.qpid.client;

import javax.transaction.xa.XAException;
import javax.transaction.xa.XAResource;
import javax.transaction.xa.Xid;

import org.apache.qpid.QpidException;
import org.apache.qpid.dtx.XidImpl;
import org.apache.qpid.transport.*;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This is an implementation of javax.njms.XAResource.
 */
public class XAResourceImpl implements XAResource
{
    /**
     * this XAResourceImpl's logger
     */
    private static final Logger _logger = LoggerFactory.getLogger(XAResourceImpl.class);

    /**
     * Reference to the associated XASession
     */
    private XASessionImpl _xaSession = null;

    /**
     * The XID of this resource
     */
    private Xid _xid;

    //--- constructor

    /**
     * Create an XAResource associated with a XASession
     *
     * @param xaSession The session XAresource
     */
    protected XAResourceImpl(XASessionImpl xaSession)
    {
        _xaSession = xaSession;
    }

    //--- The XAResource
    /**
     * Commits the global transaction specified by xid.
     *
     * @param xid A global transaction identifier
     * @param b   If true, use a one-phase commit protocol to commit the work done on behalf of xid.
     * @throws XAException An error has occurred. An error has occurred. Possible XAExceptions are XA_HEURHAZ,
     *                     XA_HEURCOM, XA_HEURRB, XA_HEURMIX, XAER_RMERR, XAER_RMFAIL, XAER_NOTA, XAER_INVAL, or XAER_PROTO.
     */
    public void commit(Xid xid, boolean b) throws XAException
    {
        if (_logger.isDebugEnabled())
        {
            _logger.debug("commit tx branch with xid:  ", xid);
        }
        Future<XaResult> future =
                _xaSession.getQpidSession().dtxCommit(convertXid(xid), b ? Option.ONE_PHASE : Option.NONE);

        // now wait on the future for the result
        XaResult result = null;
        try
        {
            result = future.get();
        }
        catch (SessionException e)
        {
            // we need to restore the qpid session that has been closed
            _xaSession.createSession();
            convertExecutionErrorToXAErr(e.getException().getErrorCode());
        }
        checkStatus(result.getStatus());
    }

    /**
     * Ends the work performed on behalf of a transaction branch.
     * The resource manager disassociates the XA resource from the transaction branch specified
     * and lets the transaction complete.
     * <ul>
     * <li> If TMSUSPEND is specified in the flags, the transaction branch is temporarily suspended in an incomplete state.
     * The transaction context is in a suspended state and must be resumed via the start method with TMRESUME specified.
     * <li> If TMFAIL is specified, the portion of work has failed. The resource manager may mark the transaction as rollback-only
     * <li> If TMSUCCESS is specified, the portion of work has completed successfully.
     * /ul>
     *
     * @param xid  A global transaction identifier that is the same as the identifier used previously in the start method
     * @param flag One of TMSUCCESS, TMFAIL, or TMSUSPEND.
     * @throws XAException An error has occurred. An error has occurred. Possible XAException values are XAER_RMERR,
     *                     XAER_RMFAILED, XAER_NOTA, XAER_INVAL, XAER_PROTO, or XA_RB*.
     */
    public void end(Xid xid, int flag) throws XAException
    {
        if (_logger.isDebugEnabled())
        {
            _logger.debug("end tx branch with xid: ", xid);
        }
        switch (flag)
        {
            case(XAResource.TMSUCCESS):
                break;
            case(XAResource.TMFAIL):
                break;
            case(XAResource.TMSUSPEND):
                break;
            default:
                 throw new XAException(XAException.XAER_INVAL);
        }
        _xaSession.flushAcknowledgments();
        Future<XaResult> future = _xaSession.getQpidSession()
                .dtxEnd(convertXid(xid),
                        flag == XAResource.TMFAIL ? Option.FAIL : Option.NONE,
                        flag == XAResource.TMSUSPEND ? Option.SUSPEND : Option.NONE);
        // now wait on the future for the result
        XaResult result = null;
        try
        {
            result = future.get();
        }
        catch (SessionException e)
        {
            // we need to restore the qpid session that has been closed
            _xaSession.createSession();
            convertExecutionErrorToXAErr(e.getException().getErrorCode());
        }
        checkStatus(result.getStatus());
    }


    /**
     * Tells the resource manager to forget about a heuristically completed transaction branch.
     *
     * @param xid String(xid.getGlobalTransactionId() A global transaction identifier
     * @throws XAException An error has occurred. Possible exception values are XAER_RMERR, XAER_RMFAIL,
     *                     XAER_NOTA, XAER_INVAL, or XAER_PROTO.
     */
    public void forget(Xid xid) throws XAException
    {
        if (_logger.isDebugEnabled())
        {
            _logger.debug("forget tx branch with xid: ", xid);
        }
        _xaSession.getQpidSession().dtxForget(convertXid(xid));
        try
        {
            _xaSession.getQpidSession().sync();
        }
        catch (SessionException e)
        {
            // we need to restore the qpid session that has been closed
            _xaSession.createSession();
            convertExecutionErrorToXAErr(e.getException().getErrorCode());
        }
    }


    /**
     * Obtains the current transaction timeout value set for this XAResource instance.
     * If XAResource.setTransactionTimeout was not used prior to invoking this method,
     * the return value is the default timeout i.e. 0;
     * otherwise, the value used in the previous setTransactionTimeout call is returned.
     *
     * @return The transaction timeout value in seconds.
     * @throws XAException An error has occurred. Possible exception values are XAER_RMERR, XAER_RMFAIL.
     */
    public int getTransactionTimeout() throws XAException
    {
        int result = 0;
        if (_xid != null)
        {
            Future<GetTimeoutResult> future =
                    _xaSession.getQpidSession().dtxGetTimeout(convertXid(_xid));
            try
            {
                result = (int) future.get().getTimeout();
            }
            catch (SessionException e)
            {
                // we need to restore the qpid session that has been closed
                _xaSession.createSession();
                convertExecutionErrorToXAErr(e.getException().getErrorCode());
            }
        }
        return result;
    }

    /**
     * This method is called to determine if the resource manager instance represented
     * by the target object is the same as the resouce manager instance represented by
     * the parameter xaResource.
     *
     * @param xaResource An XAResource object whose resource manager instance is to
     *                   be compared with the resource manager instance of the target object
     * @return <code>true</code> if it's the same RM instance; otherwise <code>false</code>.
     * @throws XAException An error has occurred. Possible exception values are XAER_RMERR, XAER_RMFAIL.
     */
    public boolean isSameRM(XAResource xaResource) throws XAException
    {
        // TODO : get the server identity of xaResource and compare it with our own one
        return false;
    }

    /**
     * Prepare for a transaction commit of the transaction specified in <code>Xid</code>.
     *
     * @param xid A global transaction identifier.
     * @return A value indicating the resource manager's vote on the outcome of the transaction.
     *         The possible values are: XA_RDONLY or XA_OK.
     * @throws XAException An error has occurred. Possible exception values are: XAER_RMERR or XAER_NOTA
     */
    public int prepare(Xid xid) throws XAException
    {
        if (_logger.isDebugEnabled())
        {
            _logger.debug("prepare ", xid);
        }
        Future<XaResult> future = _xaSession.getQpidSession().dtxPrepare(convertXid(xid));
        XaResult result = null;
        try
        {
            result = future.get();
        }
        catch (SessionException e)
        {
            // we need to restore the qpid session that has been closed
            _xaSession.createSession();
            convertExecutionErrorToXAErr(e.getException().getErrorCode());
        }
        DtxXaStatus status = result.getStatus();
        int outcome = XAResource.XA_OK;
        switch (status)
        {
            case XA_OK:
                break;
            case XA_RDONLY:
                outcome = XAResource.XA_RDONLY;
                break;
            default:
                checkStatus(status);
        }
        return outcome;
    }

    /**
     * Obtains a list of prepared transaction branches.
     * <p/>
     * The transaction manager calls this method during recovery to obtain the list of transaction branches
     * that are currently in prepared or heuristically completed states.
     *
     * @param flag One of TMSTARTRSCAN, TMENDRSCAN, TMNOFLAGS.
     *             TMNOFLAGS must be used when no other flags are set in the parameter.
     * @return zero or more XIDs of the transaction branches that are currently in a prepared or heuristically
     *         completed state.
     * @throws XAException An error has occurred. Possible value is XAER_INVAL.
     */
    public Xid[] recover(int flag) throws XAException
    {
        // the flag is ignored
        Future<RecoverResult> future = _xaSession.getQpidSession().dtxRecover();
        RecoverResult res = null;
        try
        {
            res = future.get();
        }
        catch (SessionException e)
        {
            // we need to restore the qpid session that has been closed
            _xaSession.createSession();
            convertExecutionErrorToXAErr( e.getException().getErrorCode());
        }
        Xid[] result = new Xid[res.getInDoubt().size()];
        int i = 0;
        for (Object obj : res.getInDoubt())
        {
            org.apache.qpid.transport.Xid xid = (org.apache.qpid.transport.Xid) obj;
            result[i] = new XidImpl(xid.getBranchId(), (int) xid.getFormat(), xid.getGlobalId());
            i++;
        }
        return result;
    }

    /**
     * Informs the resource manager to roll back work done on behalf of a transaction branch
     *
     * @param xid A global transaction identifier.
     * @throws XAException An error has occurred.
     */
    public void rollback(Xid xid) throws XAException
    {
        if (_logger.isDebugEnabled())
        {
            _logger.debug("rollback tx branch with xid: ", xid);
        }

        Future<XaResult> future = _xaSession.getQpidSession().dtxRollback(convertXid(xid));
        // now wait on the future for the result
        XaResult result = null;
        try
        {
            result = future.get();
        }
        catch (SessionException e)
        {
            // we need to restore the qpid session that has been closed
            _xaSession.createSession();
            convertExecutionErrorToXAErr( e.getException().getErrorCode());
        }
        checkStatus(result.getStatus());
    }

    /**
     * Sets the current transaction timeout value for this XAResource instance.
     * Once set, this timeout value is effective until setTransactionTimeout is
     * invoked again with a different value.
     * To reset the timeout value to the default value used by the resource manager, set the value to zero.
     *
     * @param timeout The transaction timeout value in seconds.
     * @return true if transaction timeout value is set successfully; otherwise false.
     * @throws XAException An error has occurred. Possible exception values are XAER_RMERR, XAER_RMFAIL, or XAER_INVAL.
     */
    public boolean setTransactionTimeout(int timeout) throws XAException
    {
        boolean result = false;
        if (_xid != null)
        {
            try
            {
                _xaSession.getQpidSession()
                        .dtxSetTimeout(XidImpl.convert(_xid), timeout);
            }
            catch (QpidException e)
            {
                if (_logger.isDebugEnabled())
                {
                    _logger.debug("Cannot convert Xid into String format ", e);
                }
                throw new XAException(XAException.XAER_PROTO);
            }
            result = true;
        }
        return result;
    }

    /**
     * Starts work on behalf of a transaction branch specified in xid.
     * <ul>
     * <li> If TMJOIN is specified, an exception is thrown as it is not supported
     * <li> If TMRESUME is specified, the start applies to resuming a suspended transaction specified in the parameter xid.
     * <li> If neither TMJOIN nor TMRESUME is specified and the transaction specified by xid has previously been seen by the
     * resource manager, the resource manager throws the XAException exception with XAER_DUPID error code.
     * </ul>
     *
     * @param xid  A global transaction identifier to be associated with the resource
     * @param flag One of TMNOFLAGS, TMJOIN, or TMRESUME
     * @throws XAException An error has occurred. Possible exceptions
     *                     are XA_RB*, XAER_RMERR, XAER_RMFAIL, XAER_DUPID, XAER_OUTSIDE, XAER_NOTA, XAER_INVAL, or XAER_PROTO.
     */
    public void start(Xid xid, int flag) throws XAException
    {
        if (_logger.isDebugEnabled())
        {
            _logger.debug("start tx branch with xid: ", xid);
        }
        switch (flag)
        {
            case(XAResource.TMNOFLAGS):
                break;
            case(XAResource.TMJOIN):
                break;
            case(XAResource.TMRESUME):
                break;
            default:
                 throw new XAException(XAException.XAER_INVAL);
        }
        Future<XaResult> future = _xaSession.getQpidSession()
                .dtxStart(convertXid(xid),
                        flag == XAResource.TMJOIN ? Option.JOIN : Option.NONE,
                        flag == XAResource.TMRESUME ? Option.RESUME : Option.NONE);
        // now wait on the future for the result
        XaResult result = null;
        try
        {
            result = future.get();
        }
        catch (SessionException e)
        {
            // we need to restore the qpid session that has been closed
            _xaSession.createSession();
            convertExecutionErrorToXAErr(e.getException().getErrorCode());
            // TODO: The amqp spec does not allow to make the difference
            // between an already known XID and a wrong arguments (join and resume are set)
            // TODO: make sure amqp addresses that
        }
        checkStatus(result.getStatus());
        _xid = xid;
    }

    //------------------------------------------------------------------------
    // Private methods
    //------------------------------------------------------------------------

    /**
     * Check xa method outcome and, when required, convert the status into the corresponding xa exception
     * @param status method status code
     * @throws XAException corresponding XA Exception when required
     */
    private void checkStatus(DtxXaStatus status) throws XAException
    {
        switch (status)
        {
            case XA_OK:
                // Do nothing this ok
                break;
            case XA_RBROLLBACK:
                // The tx has been rolled back for an unspecified reason.
                throw new XAException(XAException.XA_RBROLLBACK);
            case XA_RBTIMEOUT:
                // The transaction branch took too long.
                throw new XAException(XAException.XA_RBTIMEOUT);
            case XA_HEURHAZ:
                // The transaction branch may have been heuristically completed.
                throw new XAException(XAException.XA_HEURHAZ);
            case XA_HEURCOM:
                // The transaction branch has been heuristically committed.
                throw new XAException(XAException.XA_HEURCOM);
            case XA_HEURRB:
                // The transaction branch has been heuristically rolled back.
                throw new XAException(XAException.XA_HEURRB);
            case XA_HEURMIX:
                // The transaction branch has been heuristically committed and rolled back.
                throw new XAException(XAException.XA_HEURMIX);
            case XA_RDONLY:
                // The transaction branch was read-only and has been committed.
                throw new XAException(XAException.XA_RDONLY);
            default:
                // this should not happen
                if (_logger.isDebugEnabled())
                {
                    _logger.debug("got unexpected status value: ", status);
                }
                //A resource manager error has occured in the transaction branch.
                throw new XAException(XAException.XAER_RMERR);
        }
    }

    /**
     * Convert execution error to xa exception.
     * @param error the execution error code
     * @throws XAException
     */
    private void convertExecutionErrorToXAErr(ExecutionErrorCode error) throws XAException
    {
        switch (error)
        {
            case NOT_ALLOWED:
                // The XID already exists.
                throw new XAException(XAException.XAER_DUPID);
            case NOT_FOUND:
                // The XID is not valid.
                throw new XAException(XAException.XAER_NOTA);
            case ILLEGAL_STATE:
                // Routine was invoked in an inproper context.
                throw new XAException(XAException.XAER_PROTO);
            case NOT_IMPLEMENTED:
                // the command is not implemented
                throw new XAException(XAException.XAER_RMERR);
            case COMMAND_INVALID:
                // Invalid call
                throw new XAException(XAException.XAER_INVAL);
            default:
                // this should not happen
                if (_logger.isDebugEnabled())
                {
                    _logger.debug("Got unexpected error: " + error);
                }
                //A resource manager error has occured in the transaction branch.
                throw new XAException(XAException.XAER_RMERR);
        }
    }

    /**
     * convert a generic xid into qpid format
     * @param xid xid to be converted
     * @return the qpid formated xid
     * @throws XAException when xid is null or when it cannot be converted. 
     */
    private org.apache.qpid.transport.Xid convertXid(Xid xid) throws XAException
    {
        if (xid == null)
        {
            // Invalid arguments were given.
            throw new XAException(XAException.XAER_INVAL);
        }
        try
        {
            return XidImpl.convert(xid);
        }
        catch (QpidException e)
        {
            if (_logger.isDebugEnabled())
            {
                _logger.debug("Cannot convert Xid into String format ", e);
            }
            //A resource manager error has occured in the transaction branch.
            throw new XAException(XAException.XAER_RMERR);
        }
    }

}
