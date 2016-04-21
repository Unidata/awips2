typedef struct         {
                         short int mesg_code;
                         short int mesg_date;
                         int mesg_time;
                         int mesg_len;
                         short int src_id;
                         short int dest_id;
                         short int num_blks;
                       } mesg_hdr_struct;

mesg_hdr_struct          MesgHdr;
