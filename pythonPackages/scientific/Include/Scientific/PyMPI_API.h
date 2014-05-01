/*
 * C API functions
 */

#define PyMPICommunicator_Type_NUM 0

#define PyMPIRequest_Type_NUM 1

#define PyMPI_DuplicateCommunicator_RET PyObject *
#define PyMPI_DuplicateCommunicator_PROTO Py_PROTO((PyMPICommunicatorObject *comm))
#define PyMPI_DuplicateCommunicator_NUM 2

#define PyMPI_SubsetCommunicator_RET PyObject *
#define PyMPI_SubsetCommunicator_PROTO Py_PROTO((PyMPICommunicatorObject *comm, PyArrayObject *array))
#define PyMPI_SubsetCommunicator_NUM 3

#define PyMPI_Barrier_RET int
#define PyMPI_Barrier_PROTO Py_PROTO((PyMPICommunicatorObject *comm))
#define PyMPI_Barrier_NUM 4

#define PyMPI_Send_RET int
#define PyMPI_Send_PROTO Py_PROTO((PyMPICommunicatorObject *comm, void *data, int mpi_type, int len, int dest, int tag))
#define PyMPI_Send_NUM 5

#define PyMPI_SendArray_RET int
#define PyMPI_SendArray_PROTO Py_PROTO((PyMPICommunicatorObject *comm, PyArrayObject *array, int dest, int tag))
#define PyMPI_SendArray_NUM 6

#define PyMPI_SendString_RET int
#define PyMPI_SendString_PROTO Py_PROTO((PyMPICommunicatorObject *comm, PyStringObject *string, int dest, int tag))
#define PyMPI_SendString_NUM 7

#define PyMPI_SendArrayNonBlocking_RET PyObject *
#define PyMPI_SendArrayNonBlocking_PROTO Py_PROTO((PyMPICommunicatorObject *comm, PyArrayObject *array, int dest, int tag))
#define PyMPI_SendArrayNonBlocking_NUM 8

#define PyMPI_SendStringNonBlocking_RET PyObject *
#define PyMPI_SendStringNonBlocking_PROTO Py_PROTO((PyMPICommunicatorObject *comm, PyStringObject *string, int dest, int tag))
#define PyMPI_SendStringNonBlocking_NUM 9

#define PyMPI_Receive_RET int
#define PyMPI_Receive_PROTO Py_PROTO((PyMPICommunicatorObject *comm, void *buffer, int mpi_type, int len, int source, int tag, int *sourcep, int *tagp, int *lenp))
#define PyMPI_Receive_NUM 10

#define PyMPI_ReceiveArray_RET int
#define PyMPI_ReceiveArray_PROTO Py_PROTO((PyMPICommunicatorObject *comm, PyArrayObject *array, int source, int tag, int *sourcep, int *tagp, int *lenp))
#define PyMPI_ReceiveArray_NUM 11

#define PyMPI_ReceiveString_RET PyObject *
#define PyMPI_ReceiveString_PROTO Py_PROTO((PyMPICommunicatorObject *comm, int source, int tag, int *sourcep, int *tagp))
#define PyMPI_ReceiveString_NUM 12

#define PyMPI_ReceiveArrayNonBlocking_RET PyObject *
#define PyMPI_ReceiveArrayNonBlocking_PROTO Py_PROTO((PyMPICommunicatorObject *comm, PyArrayObject *array, int source, int tag))
#define PyMPI_ReceiveArrayNonBlocking_NUM 13

#define PyMPI_ProbeNonBlocking_RET int
#define PyMPI_ProbeNonBlocking_PROTO Py_PROTO((PyMPICommunicatorObject *comm, int source, int tag, int *flagp, int *sourcep, int *tagp))
#define PyMPI_ProbeNonBlocking_NUM 14

#define PyMPI_Broadcast_RET int
#define PyMPI_Broadcast_PROTO Py_PROTO((PyMPICommunicatorObject *comm, void *buffer, int mpi_type, int count, int root))
#define PyMPI_Broadcast_NUM 15

#define PyMPI_BroadcastArray_RET int
#define PyMPI_BroadcastArray_PROTO Py_PROTO((PyMPICommunicatorObject *comm, PyArrayObject *array, int root))
#define PyMPI_BroadcastArray_NUM 16

#define PyMPI_Share_RET int
#define PyMPI_Share_PROTO Py_PROTO((PyMPICommunicatorObject *comm, void *send, void *receive, int mpi_type, int count))
#define PyMPI_Share_NUM 17

#define PyMPI_ShareArray_RET int
#define PyMPI_ShareArray_PROTO Py_PROTO((PyMPICommunicatorObject *comm, PyArrayObject *send, PyArrayObject *receive))
#define PyMPI_ShareArray_NUM 18

#define PyMPI_Abort_RET int
#define PyMPI_Abort_PROTO Py_PROTO((PyMPICommunicatorObject *comm, int err))
#define PyMPI_Abort_NUM 19

#define PyMPI_Reduce_RET int
#define PyMPI_Reduce_PROTO Py_PROTO((PyMPICommunicatorObject *comm, void *sendbuf, void *recvbuf, int count, int datatype, PyMPIOperationObject *op, int root))
#define PyMPI_Reduce_NUM 20

#define PyMPI_Allreduce_RET int
#define PyMPI_Allreduce_PROTO Py_PROTO((PyMPICommunicatorObject *comm, void *sendbuf, void *recvbuf, int count, int datatype, PyMPIOperationObject *op))
#define PyMPI_Allreduce_NUM 21

#define PyMPI_ReduceArray_RET int
#define PyMPI_ReduceArray_PROTO Py_PROTO((PyMPICommunicatorObject *comm, PyArrayObject *send, PyArrayObject *receive, PyMPIOperationObject *op, int root))
#define PyMPI_ReduceArray_NUM 22

#define PyMPI_AllreduceArray_RET int
#define PyMPI_AllreduceArray_PROTO Py_PROTO((PyMPICommunicatorObject *comm, PyArrayObject *send, PyArrayObject *receive, PyMPIOperationObject *op))
#define PyMPI_AllreduceArray_NUM 23

#define PyMPI_Wait_RET int
#define PyMPI_Wait_PROTO Py_PROTO((PyMPIRequestObject *comm, void *s))
#define PyMPI_Wait_NUM 24

#define PyMPI_Test_RET int
#define PyMPI_Test_PROTO Py_PROTO((PyMPIRequestObject *comm, int *flag, void *s))
#define PyMPI_Test_NUM 25

#define MPI_Abort_RET int
#define MPI_Abort_PROTO Py_PROTO((MPI_Comm comm, int errorcode))
#define MPI_Abort_NUM 26

#define MPI_Allgather_RET int
#define MPI_Allgather_PROTO Py_PROTO((void *sendbuffer, int sendcount, MPI_Datatype sendtype, void *recvbuf, int recvcount, MPI_Datatype recvtype, MPI_Comm comm))
#define MPI_Allgather_NUM 27

#define MPI_Allgatherv_RET int
#define MPI_Allgatherv_PROTO Py_PROTO((void *sendbuffer, int sendcount, MPI_Datatype sendtype, void *recvbuf, int *recvcount, int *displs, MPI_Datatype recvtype, MPI_Comm comm))
#define MPI_Allgatherv_NUM 28

#define MPI_Allreduce_RET int
#define MPI_Allreduce_PROTO Py_PROTO((void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype, MPI_Op op, MPI_Comm comm))
#define MPI_Allreduce_NUM 29

#define MPI_Alltoall_RET int
#define MPI_Alltoall_PROTO Py_PROTO((void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf, int recvcount, MPI_Datatype recvtype, MPI_Comm comm))
#define MPI_Alltoall_NUM 30

#define MPI_Alltoallv_RET int
#define MPI_Alltoallv_PROTO Py_PROTO((void *sendbuf, int *sendcounts, int *sdispls, MPI_Datatype sendtype, void *recvbuf, int *recvcounts, int *rdispls, MPI_Datatype recvtype, MPI_Comm comm))
#define MPI_Alltoallv_NUM 31

#define MPI_Barrier_RET int
#define MPI_Barrier_PROTO Py_PROTO((MPI_Comm comm))
#define MPI_Barrier_NUM 32

#define MPI_Bcast_RET int
#define MPI_Bcast_PROTO Py_PROTO((void *buffer, int count, MPI_Datatype datatype, int root, MPI_Comm comm))
#define MPI_Bcast_NUM 33

#define MPI_Bsend_RET int
#define MPI_Bsend_PROTO Py_PROTO((void *buf, int count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm))
#define MPI_Bsend_NUM 34

#define MPI_Comm_dup_RET int
#define MPI_Comm_dup_PROTO Py_PROTO((MPI_Comm comm, MPI_Comm *newcomm))
#define MPI_Comm_dup_NUM 35

#define MPI_Comm_group_RET int
#define MPI_Comm_group_PROTO Py_PROTO((MPI_Comm comm, MPI_Group *group))
#define MPI_Comm_group_NUM 36

#define MPI_Group_incl_RET int
#define MPI_Group_incl_PROTO Py_PROTO((MPI_Group group, int n, int *ranks, MPI_Group *newgroup))
#define MPI_Group_incl_NUM 37

#define MPI_Comm_create_RET int
#define MPI_Comm_create_PROTO Py_PROTO((MPI_Comm comm, MPI_Group group, MPI_Comm *newcomm))
#define MPI_Comm_create_NUM 38

#define MPI_Group_free_RET int
#define MPI_Group_free_PROTO Py_PROTO((MPI_Group *group))
#define MPI_Group_free_NUM 39

#define MPI_Comm_free_RET int
#define MPI_Comm_free_PROTO Py_PROTO((MPI_Comm *comm))
#define MPI_Comm_free_NUM 40

#define MPI_Comm_rank_RET int
#define MPI_Comm_rank_PROTO Py_PROTO((MPI_Comm comm, int *rank))
#define MPI_Comm_rank_NUM 41

#define MPI_Comm_size_RET int
#define MPI_Comm_size_PROTO Py_PROTO((MPI_Comm comm, int *size))
#define MPI_Comm_size_NUM 42

#define MPI_Error_string_RET int
#define MPI_Error_string_PROTO Py_PROTO((int errorcode, char *string, int *resultlen))
#define MPI_Error_string_NUM 43

#define MPI_Finalize_RET int
#define MPI_Finalize_PROTO Py_PROTO((void))
#define MPI_Finalize_NUM 44

#define MPI_Gather_RET int
#define MPI_Gather_PROTO Py_PROTO((void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf, int recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm))
#define MPI_Gather_NUM 45

#define MPI_Gatherv_RET int
#define MPI_Gatherv_PROTO Py_PROTO((void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf, int *recvcounts, int *displs, MPI_Datatype recvtype, int root, MPI_Comm comm))
#define MPI_Gatherv_NUM 46

#define MPI_Get_count_RET int
#define MPI_Get_count_PROTO Py_PROTO((MPI_Status *status, MPI_Datatype datatype, int *count))
#define MPI_Get_count_NUM 47

#define MPI_Ibsend_RET int
#define MPI_Ibsend_PROTO Py_PROTO((void *buf, int count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm, MPI_Request *request))
#define MPI_Ibsend_NUM 48

#define MPI_Init_RET int
#define MPI_Init_PROTO Py_PROTO((int *argc, char ***argv))
#define MPI_Init_NUM 49

#define MPI_Initialized_RET int
#define MPI_Initialized_PROTO Py_PROTO((int *flag))
#define MPI_Initialized_NUM 50

#define MPI_Iprobe_RET int
#define MPI_Iprobe_PROTO Py_PROTO((int source, int tag, MPI_Comm comm, int *flag, MPI_Status *status))
#define MPI_Iprobe_NUM 51

#define MPI_Irecv_RET int
#define MPI_Irecv_PROTO Py_PROTO((void *buf, int count, MPI_Datatype datatype, int source, int tag, MPI_Comm comm, MPI_Request *request))
#define MPI_Irecv_NUM 52

#define MPI_Irsend_RET int
#define MPI_Irsend_PROTO Py_PROTO((void *buf, int count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm, MPI_Request *request))
#define MPI_Irsend_NUM 53

#define MPI_Isend_RET int
#define MPI_Isend_PROTO Py_PROTO((void *buf, int count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm, MPI_Request *request))
#define MPI_Isend_NUM 54

#define MPI_Issend_RET int
#define MPI_Issend_PROTO Py_PROTO((void *buf, int count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm, MPI_Request *request))
#define MPI_Issend_NUM 55

#define MPI_Probe_RET int
#define MPI_Probe_PROTO Py_PROTO((int source, int tag, MPI_Comm comm, MPI_Status *status))
#define MPI_Probe_NUM 56

#define MPI_Recv_RET int
#define MPI_Recv_PROTO Py_PROTO((void *buf, int count, MPI_Datatype datatype, int source, int tag, MPI_Comm comm, MPI_Status *status))
#define MPI_Recv_NUM 57

#define MPI_Reduce_RET int
#define MPI_Reduce_PROTO Py_PROTO((void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype, MPI_Op op, int root, MPI_Comm comm))
#define MPI_Reduce_NUM 58

#define MPI_Reduce_scatter_RET int
#define MPI_Reduce_scatter_PROTO Py_PROTO((void *sendbuf, void *recvbuf, int *recvcounts, MPI_Datatype datatype, MPI_Op op, MPI_Comm comm))
#define MPI_Reduce_scatter_NUM 59

#define MPI_Rsend_RET int
#define MPI_Rsend_PROTO Py_PROTO((void *buf, int count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm))
#define MPI_Rsend_NUM 60

#define MPI_Scatter_RET int
#define MPI_Scatter_PROTO Py_PROTO((void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf, int recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm))
#define MPI_Scatter_NUM 61

#define MPI_Scatterv_RET int
#define MPI_Scatterv_PROTO Py_PROTO((void *sendbuf, int *sendcounts, int *displs, MPI_Datatype sendtype, void *recvbuf, int recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm))
#define MPI_Scatterv_NUM 62

#define MPI_Send_RET int
#define MPI_Send_PROTO Py_PROTO((void *buf, int count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm))
#define MPI_Send_NUM 63

#define MPI_Sendrecv_RET int
#define MPI_Sendrecv_PROTO Py_PROTO((void *sendbuf, int sendcount, MPI_Datatype sendtype, int dest, int sendtag, void *recvbuf, int recvcount, MPI_Datatype recvtype, int source, int recvtag, MPI_Comm comm, MPI_Status *status))
#define MPI_Sendrecv_NUM 64

#define MPI_Ssend_RET int
#define MPI_Ssend_PROTO Py_PROTO((void *buf, int count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm))
#define MPI_Ssend_NUM 65

#define MPI_Test_RET int
#define MPI_Test_PROTO Py_PROTO((MPI_Request *request, int *flag, MPI_Status *status))
#define MPI_Test_NUM 66

#define MPI_Testall_RET int
#define MPI_Testall_PROTO Py_PROTO((int count, MPI_Request *array_of_requests, int *flag, MPI_Status *array_of_statuses))
#define MPI_Testall_NUM 67

#define MPI_Testany_RET int
#define MPI_Testany_PROTO Py_PROTO((int count, MPI_Request *array_of_requests, int *index, int *flag, MPI_Status *status))
#define MPI_Testany_NUM 68

#define MPI_Wait_RET int
#define MPI_Wait_PROTO Py_PROTO((MPI_Request *request, MPI_Status *status))
#define MPI_Wait_NUM 69

#define MPI_Waitall_RET int
#define MPI_Waitall_PROTO Py_PROTO((int count, MPI_Request *array_of_requests, MPI_Status *array_of_statuses))
#define MPI_Waitall_NUM 70

#define MPI_Waitany_RET int
#define MPI_Waitany_PROTO Py_PROTO((int count, MPI_Request *array_of_requests, int *index, MPI_Status *array_of_statuses))
#define MPI_Waitany_NUM 71

#define MPI_Wtick_RET double
#define MPI_Wtick_PROTO Py_PROTO((void))
#define MPI_Wtick_NUM 72

#define MPI_Wtime_RET double
#define MPI_Wtime_PROTO Py_PROTO((void))
#define MPI_Wtime_NUM 73

#define PyMPI_API_pointers 74

#ifdef _MPI_MODULE

statichere PyTypeObject PyMPICommunicator_Type;
#define PyMPICommunicator_Check(op) ((op)->ob_type == &PyMPICommunicator_Type)

statichere PyTypeObject PyMPIRequest_Type;
#define PyMPIRequest_Check(op) ((op)->ob_type == &PyMPIRequest_Type)

static PyMPI_DuplicateCommunicator_RET PyMPI_DuplicateCommunicator PyMPI_DuplicateCommunicator_PROTO;

static PyMPI_SubsetCommunicator_RET PyMPI_SubsetCommunicator PyMPI_SubsetCommunicator_PROTO;

static PyMPI_Barrier_RET PyMPI_Barrier PyMPI_Barrier_PROTO;

static PyMPI_Send_RET PyMPI_Send PyMPI_Send_PROTO;

static PyMPI_SendArray_RET PyMPI_SendArray PyMPI_SendArray_PROTO;

static PyMPI_SendString_RET PyMPI_SendString PyMPI_SendString_PROTO;

static PyMPI_SendArrayNonBlocking_RET PyMPI_SendArrayNonBlocking PyMPI_SendArrayNonBlocking_PROTO;

static PyMPI_SendStringNonBlocking_RET PyMPI_SendStringNonBlocking PyMPI_SendStringNonBlocking_PROTO;

static PyMPI_Receive_RET PyMPI_Receive PyMPI_Receive_PROTO;

static PyMPI_ReceiveArray_RET PyMPI_ReceiveArray PyMPI_ReceiveArray_PROTO;

static PyMPI_ReceiveString_RET PyMPI_ReceiveString PyMPI_ReceiveString_PROTO;

static PyMPI_ReceiveArrayNonBlocking_RET PyMPI_ReceiveArrayNonBlocking PyMPI_ReceiveArrayNonBlocking_PROTO;

static PyMPI_ProbeNonBlocking_RET PyMPI_ProbeNonBlocking PyMPI_ProbeNonBlocking_PROTO;

static PyMPI_Broadcast_RET PyMPI_Broadcast PyMPI_Broadcast_PROTO;

static PyMPI_BroadcastArray_RET PyMPI_BroadcastArray PyMPI_BroadcastArray_PROTO;

static PyMPI_Share_RET PyMPI_Share PyMPI_Share_PROTO;

static PyMPI_ShareArray_RET PyMPI_ShareArray PyMPI_ShareArray_PROTO;

static PyMPI_Abort_RET PyMPI_Abort PyMPI_Abort_PROTO;

static PyMPI_Reduce_RET PyMPI_Reduce PyMPI_Reduce_PROTO;

static PyMPI_Allreduce_RET PyMPI_Allreduce PyMPI_Allreduce_PROTO;

static PyMPI_ReduceArray_RET PyMPI_ReduceArray PyMPI_ReduceArray_PROTO;

static PyMPI_AllreduceArray_RET PyMPI_AllreduceArray PyMPI_AllreduceArray_PROTO;

static PyMPI_Wait_RET PyMPI_Wait PyMPI_Wait_PROTO;

static PyMPI_Test_RET PyMPI_Test PyMPI_Test_PROTO;

#define set_PyMPI_API_pointers(){ \
   PyMPI_API[PyMPICommunicator_Type_NUM] = (void *)&PyMPICommunicator_Type; \
   PyMPI_API[PyMPIRequest_Type_NUM] = (void *)&PyMPIRequest_Type; \
   PyMPI_API[PyMPI_DuplicateCommunicator_NUM] = (void *)&PyMPI_DuplicateCommunicator; \
   PyMPI_API[PyMPI_SubsetCommunicator_NUM] = (void *)&PyMPI_SubsetCommunicator; \
   PyMPI_API[PyMPI_Barrier_NUM] = (void *)&PyMPI_Barrier; \
   PyMPI_API[PyMPI_Send_NUM] = (void *)&PyMPI_Send; \
   PyMPI_API[PyMPI_SendArray_NUM] = (void *)&PyMPI_SendArray; \
   PyMPI_API[PyMPI_SendString_NUM] = (void *)&PyMPI_SendString; \
   PyMPI_API[PyMPI_SendArrayNonBlocking_NUM] = (void *)&PyMPI_SendArrayNonBlocking; \
   PyMPI_API[PyMPI_SendStringNonBlocking_NUM] = (void *)&PyMPI_SendStringNonBlocking; \
   PyMPI_API[PyMPI_Receive_NUM] = (void *)&PyMPI_Receive; \
   PyMPI_API[PyMPI_ReceiveArray_NUM] = (void *)&PyMPI_ReceiveArray; \
   PyMPI_API[PyMPI_ReceiveString_NUM] = (void *)&PyMPI_ReceiveString; \
   PyMPI_API[PyMPI_ReceiveArrayNonBlocking_NUM] = (void *)&PyMPI_ReceiveArrayNonBlocking; \
   PyMPI_API[PyMPI_ProbeNonBlocking_NUM] = (void *)&PyMPI_ProbeNonBlocking; \
   PyMPI_API[PyMPI_Broadcast_NUM] = (void *)&PyMPI_Broadcast; \
   PyMPI_API[PyMPI_BroadcastArray_NUM] = (void *)&PyMPI_BroadcastArray; \
   PyMPI_API[PyMPI_Share_NUM] = (void *)&PyMPI_Share; \
   PyMPI_API[PyMPI_ShareArray_NUM] = (void *)&PyMPI_ShareArray; \
   PyMPI_API[PyMPI_Abort_NUM] = (void *)&PyMPI_Abort; \
   PyMPI_API[PyMPI_Reduce_NUM] = (void *)&PyMPI_Reduce; \
   PyMPI_API[PyMPI_Allreduce_NUM] = (void *)&PyMPI_Allreduce; \
   PyMPI_API[PyMPI_ReduceArray_NUM] = (void *)&PyMPI_ReduceArray; \
   PyMPI_API[PyMPI_AllreduceArray_NUM] = (void *)&PyMPI_AllreduceArray; \
   PyMPI_API[PyMPI_Wait_NUM] = (void *)&PyMPI_Wait; \
   PyMPI_API[PyMPI_Test_NUM] = (void *)&PyMPI_Test; \
   PyMPI_API[MPI_Abort_NUM] = (void *)&MPI_Abort; \
   PyMPI_API[MPI_Allgather_NUM] = (void *)&MPI_Allgather; \
   PyMPI_API[MPI_Allgatherv_NUM] = (void *)&MPI_Allgatherv; \
   PyMPI_API[MPI_Allreduce_NUM] = (void *)&MPI_Allreduce; \
   PyMPI_API[MPI_Alltoall_NUM] = (void *)&MPI_Alltoall; \
   PyMPI_API[MPI_Alltoallv_NUM] = (void *)&MPI_Alltoallv; \
   PyMPI_API[MPI_Barrier_NUM] = (void *)&MPI_Barrier; \
   PyMPI_API[MPI_Bcast_NUM] = (void *)&MPI_Bcast; \
   PyMPI_API[MPI_Bsend_NUM] = (void *)&MPI_Bsend; \
   PyMPI_API[MPI_Comm_dup_NUM] = (void *)&MPI_Comm_dup; \
   PyMPI_API[MPI_Comm_group_NUM] = (void *)&MPI_Comm_group; \
   PyMPI_API[MPI_Group_incl_NUM] = (void *)&MPI_Group_incl; \
   PyMPI_API[MPI_Comm_create_NUM] = (void *)&MPI_Comm_create; \
   PyMPI_API[MPI_Group_free_NUM] = (void *)&MPI_Group_free; \
   PyMPI_API[MPI_Comm_free_NUM] = (void *)&MPI_Comm_free; \
   PyMPI_API[MPI_Comm_rank_NUM] = (void *)&MPI_Comm_rank; \
   PyMPI_API[MPI_Comm_size_NUM] = (void *)&MPI_Comm_size; \
   PyMPI_API[MPI_Error_string_NUM] = (void *)&MPI_Error_string; \
   PyMPI_API[MPI_Finalize_NUM] = (void *)&MPI_Finalize; \
   PyMPI_API[MPI_Gather_NUM] = (void *)&MPI_Gather; \
   PyMPI_API[MPI_Gatherv_NUM] = (void *)&MPI_Gatherv; \
   PyMPI_API[MPI_Get_count_NUM] = (void *)&MPI_Get_count; \
   PyMPI_API[MPI_Ibsend_NUM] = (void *)&MPI_Ibsend; \
   PyMPI_API[MPI_Init_NUM] = (void *)&MPI_Init; \
   PyMPI_API[MPI_Initialized_NUM] = (void *)&MPI_Initialized; \
   PyMPI_API[MPI_Iprobe_NUM] = (void *)&MPI_Iprobe; \
   PyMPI_API[MPI_Irecv_NUM] = (void *)&MPI_Irecv; \
   PyMPI_API[MPI_Irsend_NUM] = (void *)&MPI_Irsend; \
   PyMPI_API[MPI_Isend_NUM] = (void *)&MPI_Isend; \
   PyMPI_API[MPI_Issend_NUM] = (void *)&MPI_Issend; \
   PyMPI_API[MPI_Probe_NUM] = (void *)&MPI_Probe; \
   PyMPI_API[MPI_Recv_NUM] = (void *)&MPI_Recv; \
   PyMPI_API[MPI_Reduce_NUM] = (void *)&MPI_Reduce; \
   PyMPI_API[MPI_Reduce_scatter_NUM] = (void *)&MPI_Reduce_scatter; \
   PyMPI_API[MPI_Rsend_NUM] = (void *)&MPI_Rsend; \
   PyMPI_API[MPI_Scatter_NUM] = (void *)&MPI_Scatter; \
   PyMPI_API[MPI_Scatterv_NUM] = (void *)&MPI_Scatterv; \
   PyMPI_API[MPI_Send_NUM] = (void *)&MPI_Send; \
   PyMPI_API[MPI_Sendrecv_NUM] = (void *)&MPI_Sendrecv; \
   PyMPI_API[MPI_Ssend_NUM] = (void *)&MPI_Ssend; \
   PyMPI_API[MPI_Test_NUM] = (void *)&MPI_Test; \
   PyMPI_API[MPI_Testall_NUM] = (void *)&MPI_Testall; \
   PyMPI_API[MPI_Testany_NUM] = (void *)&MPI_Testany; \
   PyMPI_API[MPI_Wait_NUM] = (void *)&MPI_Wait; \
   PyMPI_API[MPI_Waitall_NUM] = (void *)&MPI_Waitall; \
   PyMPI_API[MPI_Waitany_NUM] = (void *)&MPI_Waitany; \
   PyMPI_API[MPI_Wtick_NUM] = (void *)&MPI_Wtick; \
   PyMPI_API[MPI_Wtime_NUM] = (void *)&MPI_Wtime; \
}

#else

#ifndef PYMPI_API_LINKAGE
#define PYMPI_API_LINKAGE static
#endif

PYMPI_API_LINKAGE void **PyMPI_API;

#define PyMPICommunicator_Check(op) \
  ((op)->ob_type == (PyTypePbject *)PyMPI_API[PyMPICommunicator_Type_Num])

#define PyMPIRequest_Check(op) \
  ((op)->ob_type == (PyTypePbject *)PyMPI_API[PyMPIRequest_Type_Num])

#define PyMPI_DuplicateCommunicator \
  (*(PyMPI_DuplicateCommunicator_RET (*)PyMPI_DuplicateCommunicator_PROTO) \
   PyMPI_API[PyMPI_DuplicateCommunicator_NUM])

#define PyMPI_SubsetCommunicator \
  (*(PyMPI_SubsetCommunicator_RET (*)PyMPI_SubsetCommunicator_PROTO) \
   PyMPI_API[PyMPI_SubsetCommunicator_NUM])

#define PyMPI_Barrier \
  (*(PyMPI_Barrier_RET (*)PyMPI_Barrier_PROTO) \
   PyMPI_API[PyMPI_Barrier_NUM])

#define PyMPI_Send \
  (*(PyMPI_Send_RET (*)PyMPI_Send_PROTO) \
   PyMPI_API[PyMPI_Send_NUM])

#define PyMPI_SendArray \
  (*(PyMPI_SendArray_RET (*)PyMPI_SendArray_PROTO) \
   PyMPI_API[PyMPI_SendArray_NUM])

#define PyMPI_SendString \
  (*(PyMPI_SendString_RET (*)PyMPI_SendString_PROTO) \
   PyMPI_API[PyMPI_SendString_NUM])

#define PyMPI_SendArrayNonBlocking \
  (*(PyMPI_SendArrayNonBlocking_RET (*)PyMPI_SendArrayNonBlocking_PROTO) \
   PyMPI_API[PyMPI_SendArrayNonBlocking_NUM])

#define PyMPI_SendStringNonBlocking \
  (*(PyMPI_SendStringNonBlocking_RET (*)PyMPI_SendStringNonBlocking_PROTO) \
   PyMPI_API[PyMPI_SendStringNonBlocking_NUM])

#define PyMPI_Receive \
  (*(PyMPI_Receive_RET (*)PyMPI_Receive_PROTO) \
   PyMPI_API[PyMPI_Receive_NUM])

#define PyMPI_ReceiveArray \
  (*(PyMPI_ReceiveArray_RET (*)PyMPI_ReceiveArray_PROTO) \
   PyMPI_API[PyMPI_ReceiveArray_NUM])

#define PyMPI_ReceiveString \
  (*(PyMPI_ReceiveString_RET (*)PyMPI_ReceiveString_PROTO) \
   PyMPI_API[PyMPI_ReceiveString_NUM])

#define PyMPI_ReceiveArrayNonBlocking \
  (*(PyMPI_ReceiveArrayNonBlocking_RET (*)PyMPI_ReceiveArrayNonBlocking_PROTO) \
   PyMPI_API[PyMPI_ReceiveArrayNonBlocking_NUM])

#define PyMPI_ProbeNonBlocking \
  (*(PyMPI_ProbeNonBlocking_RET (*)PyMPI_ProbeNonBlocking_PROTO) \
   PyMPI_API[PyMPI_ProbeNonBlocking_NUM])

#define PyMPI_Broadcast \
  (*(PyMPI_Broadcast_RET (*)PyMPI_Broadcast_PROTO) \
   PyMPI_API[PyMPI_Broadcast_NUM])

#define PyMPI_BroadcastArray \
  (*(PyMPI_BroadcastArray_RET (*)PyMPI_BroadcastArray_PROTO) \
   PyMPI_API[PyMPI_BroadcastArray_NUM])

#define PyMPI_Share \
  (*(PyMPI_Share_RET (*)PyMPI_Share_PROTO) \
   PyMPI_API[PyMPI_Share_NUM])

#define PyMPI_ShareArray \
  (*(PyMPI_ShareArray_RET (*)PyMPI_ShareArray_PROTO) \
   PyMPI_API[PyMPI_ShareArray_NUM])

#define PyMPI_Abort \
  (*(PyMPI_Abort_RET (*)PyMPI_Abort_PROTO) \
   PyMPI_API[PyMPI_Abort_NUM])

#define PyMPI_Reduce \
  (*(PyMPI_Reduce_RET (*)PyMPI_Reduce_PROTO) \
   PyMPI_API[PyMPI_Reduce_NUM])

#define PyMPI_Allreduce \
  (*(PyMPI_Allreduce_RET (*)PyMPI_Allreduce_PROTO) \
   PyMPI_API[PyMPI_Allreduce_NUM])

#define PyMPI_ReduceArray \
  (*(PyMPI_ReduceArray_RET (*)PyMPI_ReduceArray_PROTO) \
   PyMPI_API[PyMPI_ReduceArray_NUM])

#define PyMPI_AllreduceArray \
  (*(PyMPI_AllreduceArray_RET (*)PyMPI_AllreduceArray_PROTO) \
   PyMPI_API[PyMPI_AllreduceArray_NUM])

#define PyMPI_Wait \
  (*(PyMPI_Wait_RET (*)PyMPI_Wait_PROTO) \
   PyMPI_API[PyMPI_Wait_NUM])

#define PyMPI_Test \
  (*(PyMPI_Test_RET (*)PyMPI_Test_PROTO) \
   PyMPI_API[PyMPI_Test_NUM])

#define MPI_Abort \
  (*(MPI_Abort_RET (*)MPI_Abort_PROTO) \
   PyMPI_API[MPI_Abort_NUM])

#define MPI_Allgather \
  (*(MPI_Allgather_RET (*)MPI_Allgather_PROTO) \
   PyMPI_API[MPI_Allgather_NUM])

#define MPI_Allgatherv \
  (*(MPI_Allgatherv_RET (*)MPI_Allgatherv_PROTO) \
   PyMPI_API[MPI_Allgatherv_NUM])

#define MPI_Allreduce \
  (*(MPI_Allreduce_RET (*)MPI_Allreduce_PROTO) \
   PyMPI_API[MPI_Allreduce_NUM])

#define MPI_Alltoall \
  (*(MPI_Alltoall_RET (*)MPI_Alltoall_PROTO) \
   PyMPI_API[MPI_Alltoall_NUM])

#define MPI_Alltoallv \
  (*(MPI_Alltoallv_RET (*)MPI_Alltoallv_PROTO) \
   PyMPI_API[MPI_Alltoallv_NUM])

#define MPI_Barrier \
  (*(MPI_Barrier_RET (*)MPI_Barrier_PROTO) \
   PyMPI_API[MPI_Barrier_NUM])

#define MPI_Bcast \
  (*(MPI_Bcast_RET (*)MPI_Bcast_PROTO) \
   PyMPI_API[MPI_Bcast_NUM])

#define MPI_Bsend \
  (*(MPI_Bsend_RET (*)MPI_Bsend_PROTO) \
   PyMPI_API[MPI_Bsend_NUM])

#define MPI_Comm_dup \
  (*(MPI_Comm_dup_RET (*)MPI_Comm_dup_PROTO) \
   PyMPI_API[MPI_Comm_dup_NUM])

#define MPI_Comm_group \
  (*(MPI_Comm_group_RET (*)MPI_Comm_group_PROTO) \
   PyMPI_API[MPI_Comm_group_NUM])

#define MPI_Group_incl \
  (*(MPI_Group_incl_RET (*)MPI_Group_incl_PROTO) \
   PyMPI_API[MPI_Group_incl_NUM])

#define MPI_Comm_create \
  (*(MPI_Comm_create_RET (*)MPI_Comm_create_PROTO) \
   PyMPI_API[MPI_Comm_create_NUM])

#define MPI_Group_free \
  (*(MPI_Group_free_RET (*)MPI_Group_free_PROTO) \
   PyMPI_API[MPI_Group_free_NUM])

#define MPI_Comm_free \
  (*(MPI_Comm_free_RET (*)MPI_Comm_free_PROTO) \
   PyMPI_API[MPI_Comm_free_NUM])

#define MPI_Comm_rank \
  (*(MPI_Comm_rank_RET (*)MPI_Comm_rank_PROTO) \
   PyMPI_API[MPI_Comm_rank_NUM])

#define MPI_Comm_size \
  (*(MPI_Comm_size_RET (*)MPI_Comm_size_PROTO) \
   PyMPI_API[MPI_Comm_size_NUM])

#define MPI_Error_string \
  (*(MPI_Error_string_RET (*)MPI_Error_string_PROTO) \
   PyMPI_API[MPI_Error_string_NUM])

#define MPI_Finalize \
  (*(MPI_Finalize_RET (*)MPI_Finalize_PROTO) \
   PyMPI_API[MPI_Finalize_NUM])

#define MPI_Gather \
  (*(MPI_Gather_RET (*)MPI_Gather_PROTO) \
   PyMPI_API[MPI_Gather_NUM])

#define MPI_Gatherv \
  (*(MPI_Gatherv_RET (*)MPI_Gatherv_PROTO) \
   PyMPI_API[MPI_Gatherv_NUM])

#define MPI_Get_count \
  (*(MPI_Get_count_RET (*)MPI_Get_count_PROTO) \
   PyMPI_API[MPI_Get_count_NUM])

#define MPI_Ibsend \
  (*(MPI_Ibsend_RET (*)MPI_Ibsend_PROTO) \
   PyMPI_API[MPI_Ibsend_NUM])

#define MPI_Init \
  (*(MPI_Init_RET (*)MPI_Init_PROTO) \
   PyMPI_API[MPI_Init_NUM])

#define MPI_Initialized \
  (*(MPI_Initialized_RET (*)MPI_Initialized_PROTO) \
   PyMPI_API[MPI_Initialized_NUM])

#define MPI_Iprobe \
  (*(MPI_Iprobe_RET (*)MPI_Iprobe_PROTO) \
   PyMPI_API[MPI_Iprobe_NUM])

#define MPI_Irecv \
  (*(MPI_Irecv_RET (*)MPI_Irecv_PROTO) \
   PyMPI_API[MPI_Irecv_NUM])

#define MPI_Irsend \
  (*(MPI_Irsend_RET (*)MPI_Irsend_PROTO) \
   PyMPI_API[MPI_Irsend_NUM])

#define MPI_Isend \
  (*(MPI_Isend_RET (*)MPI_Isend_PROTO) \
   PyMPI_API[MPI_Isend_NUM])

#define MPI_Issend \
  (*(MPI_Issend_RET (*)MPI_Issend_PROTO) \
   PyMPI_API[MPI_Issend_NUM])

#define MPI_Probe \
  (*(MPI_Probe_RET (*)MPI_Probe_PROTO) \
   PyMPI_API[MPI_Probe_NUM])

#define MPI_Recv \
  (*(MPI_Recv_RET (*)MPI_Recv_PROTO) \
   PyMPI_API[MPI_Recv_NUM])

#define MPI_Reduce \
  (*(MPI_Reduce_RET (*)MPI_Reduce_PROTO) \
   PyMPI_API[MPI_Reduce_NUM])

#define MPI_Reduce_scatter \
  (*(MPI_Reduce_scatter_RET (*)MPI_Reduce_scatter_PROTO) \
   PyMPI_API[MPI_Reduce_scatter_NUM])

#define MPI_Rsend \
  (*(MPI_Rsend_RET (*)MPI_Rsend_PROTO) \
   PyMPI_API[MPI_Rsend_NUM])

#define MPI_Scatter \
  (*(MPI_Scatter_RET (*)MPI_Scatter_PROTO) \
   PyMPI_API[MPI_Scatter_NUM])

#define MPI_Scatterv \
  (*(MPI_Scatterv_RET (*)MPI_Scatterv_PROTO) \
   PyMPI_API[MPI_Scatterv_NUM])

#define MPI_Send \
  (*(MPI_Send_RET (*)MPI_Send_PROTO) \
   PyMPI_API[MPI_Send_NUM])

#define MPI_Sendrecv \
  (*(MPI_Sendrecv_RET (*)MPI_Sendrecv_PROTO) \
   PyMPI_API[MPI_Sendrecv_NUM])

#define MPI_Ssend \
  (*(MPI_Ssend_RET (*)MPI_Ssend_PROTO) \
   PyMPI_API[MPI_Ssend_NUM])

#define MPI_Test \
  (*(MPI_Test_RET (*)MPI_Test_PROTO) \
   PyMPI_API[MPI_Test_NUM])

#define MPI_Testall \
  (*(MPI_Testall_RET (*)MPI_Testall_PROTO) \
   PyMPI_API[MPI_Testall_NUM])

#define MPI_Testany \
  (*(MPI_Testany_RET (*)MPI_Testany_PROTO) \
   PyMPI_API[MPI_Testany_NUM])

#define MPI_Wait \
  (*(MPI_Wait_RET (*)MPI_Wait_PROTO) \
   PyMPI_API[MPI_Wait_NUM])

#define MPI_Waitall \
  (*(MPI_Waitall_RET (*)MPI_Waitall_PROTO) \
   PyMPI_API[MPI_Waitall_NUM])

#define MPI_Waitany \
  (*(MPI_Waitany_RET (*)MPI_Waitany_PROTO) \
   PyMPI_API[MPI_Waitany_NUM])

#define MPI_Wtick \
  (*(MPI_Wtick_RET (*)MPI_Wtick_PROTO) \
   PyMPI_API[MPI_Wtick_NUM])

#define MPI_Wtime \
  (*(MPI_Wtime_RET (*)MPI_Wtime_PROTO) \
   PyMPI_API[MPI_Wtime_NUM])



#define import_mpi() \
{ \
  PyObject *module = PyImport_ImportModule("Scientific.MPI"); \
  if (module != NULL) { \
    PyObject *module_dict = PyModule_GetDict(module); \
    PyObject *c_api_object = PyDict_GetItemString(module_dict, "_C_API"); \
    if (PyCObject_Check(c_api_object)) { \
      PyMPI_API = (void **)PyCObject_AsVoidPtr(c_api_object); \
    } \
  } \
}

#endif
