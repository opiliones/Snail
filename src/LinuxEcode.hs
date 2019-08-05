module LinuxEcode where

ePERM           =  1      -- Operation not permitted
eNOENT          =  2      -- No such file or directory
eSRCH           =  3      -- No such process
eINTR           =  4      -- Interrupted system call
eIO             =  5      -- I/O error
eNXIO           =  6      -- No such device or address
e2BIG           =  7      -- Arg list too long
eNOEXEC         =  8      -- Exec format error
eBADF           =  9      -- Bad file number
eCHILD          = 10      -- No child processes
eAGAIN          = 11      -- Try again
eNOMEM          = 12      -- Out of memory
eACCES          = 13      -- Permission denied
eFAULT          = 14      -- Bad address
eNOTBLK         = 15      -- Block device required
eBUSY           = 16      -- Device or resource busy
eEXIST          = 17      -- File exists
eXDEV           = 18      -- Cross-device link
eNODEV          = 19      -- No such device
eNOTDIR         = 20      -- Not a directory
eISDIR          = 21      -- Is a directory
eINVAL          = 22      -- Invalid argument
eNFILE          = 23      -- File table overflow
eMFILE          = 24      -- Too many open files
eNOTTY          = 25      -- Not a typewriter
eTXTBSY         = 26      -- Text file busy
eFBIG           = 27      -- File too large
eNOSPC          = 28      -- No space left on device
eSPIPE          = 29      -- Illegal seek
eROFS           = 30      -- Read-only file system
eMLINK          = 31      -- Too many links
ePIPE           = 32      -- Broken pipe
eDOM            = 33      -- Math argument out of domain of func
eRANGE          = 34      -- Math result not representable
eDEADLK         = 35      -- Resource deadlock would occur
eNAMETOOLONG    = 36      -- File name too long
eNOLCK          = 37      -- No record locks available
eNOSYS          = 38      -- Function not implemented
eNOTEMPTY       = 39      -- Directory not empty
eLOOP           = 40      -- Too many symbolic links encountered
eWOULDBLOCK     = eAGAIN  -- Operation would block
eNOMSG          = 42      -- No message of desired type
eIDRM           = 43      -- Identifier removed
eCHRNG          = 44      -- Channel number out of range
eL2NSYNC        = 45      -- Level 2 not synchronized
eL3HLT          = 46      -- Level 3 halted
eL3RST          = 47      -- Level 3 reset
eLNRNG          = 48      -- Link number out of range
eUNATCH         = 49      -- Protocol driver not attached
eNOCSI          = 50      -- No CSI structure available
eL2HLT          = 51      -- Level 2 halted
eBADE           = 52      -- Invalid exchange
eBADR           = 53      -- Invalid request descriptor
eXFULL          = 54      -- Exchange full
eNOANO          = 55      -- No anode
eBADRQC         = 56      -- Invalid request code
eBADSLT         = 57      -- Invalid slot
eDEADLOCK       = eDEADLK
eBFONT          = 59      -- Bad font file format
eNOSTR          = 60      -- Device not a stream
eNODATA         = 61      -- No data available
eTIME           = 62      -- Timer expired
eNOSR           = 63      -- Out of streams resources
eNONET          = 64      -- Machine is not on the network
eNOPKG          = 65      -- Package not installed
eREMOTE         = 66      -- Object is remote
eNOLINK         = 67      -- Link has been severed
eADV            = 68      -- Advertise error
eSRMNT          = 69      -- Srmount error
eCOMM           = 70      -- Communication error on send
ePROTO          = 71      -- Protocol error
eMULTIHOP       = 72      -- Multihop attempted
eDOTDOT         = 73      -- RFS specific error
eBADMSG         = 74      -- Not a data message
eOVERFLOW       = 75      -- Value too large for defined data type
eNOTUNIQ        = 76      -- Name not unique on network
eBADFD          = 77      -- File descriptor in bad state
eREMCHG         = 78      -- Remote address changed
eLIBACC         = 79      -- Can not access a needed shared library
eLIBBAD         = 80      -- Accessing a corrupted shared library
eLIBSCN         = 81      -- .lib section in a.out corrupted
eLIBMAX         = 82      -- Attempting to link in too many shared libraries
eLIBEXEC        = 83      -- Cannot exec a shared library directly
eILSEQ          = 84      -- Illegal byte sequence
eRESTART        = 85      -- Interrupted system call should be restarted
eSTRPIPE        = 86      -- Streams pipe error
eUSERS          = 87      -- Too many users
eNOTSOCK        = 88      -- Socket operation on non-socket
eDESTADDRREQ    = 89      -- Destination address required
eMSGSIZE        = 90      -- Message too long
ePROTOTYPE      = 91      -- Protocol wrong type for socket
eNOPROTOOPT     = 92      -- Protocol not available
ePROTONOSUPPORT = 93      -- Protocol not supported
eSOCKTNOSUPPORT = 94      -- Socket type not supported
eOPNOTSUPP      = 95      -- Operation not supported on transport endpoint
ePFNOSUPPORT    = 96      -- Protocol family not supported
eAFNOSUPPORT    = 97      -- Address family not supported by protocol
eADDRINUSE      = 98      -- Address already in use
eADDRNOTAVAIL   = 99      -- Cannot assign requested address
eNETDOWN        = 100     -- Network is down
eNETUNREACH     = 101     -- Network is unreachable
eNETRESET       = 102     -- Network dropped connection because of reset
eCONNABORTED    = 103     -- Software caused connection abort
eCONNRESET      = 104     -- Connection reset by peer
eNOBUFS         = 105     -- No buffer space available
eISCONN         = 106     -- Transport endpoint is already connected
eNOTCONN        = 107     -- Transport endpoint is not connected
eSHUTDOWN       = 108     -- Cannot send after transport endpoint shutdown
eTOOMANYREFS    = 109     -- Too many references: cannot splice
eTIMEDOUT       = 110     -- Connection timed out
eCONNREFUSED    = 111     -- Connection refused
eHOSTDOWN       = 112     -- Host is down
eHOSTUNREACH    = 113     -- No route to host
eALREADY        = 114     -- Operation already in progress
eINPROGRESS     = 115     -- Operation now in progress
eSTALE          = 116     -- Stale NFS file handle
eUCLEAN         = 117     -- Structure needs cleaning
eNOTNAM         = 118     -- Not a XENIX named type file
eNAVAIL         = 119     -- No XENIX semaphores available
eISNAM          = 120     -- Is a named type file
eREMOTEIO       = 121     -- Remote I/O error
eDQUOT          = 122     -- Quota exceeded
eNOMEDIUM       = 123     -- No medium found
eMEDIUMTYPE     = 124     -- Wrong medium type

ePERM          :: Double
eNOENT         :: Double
eSRCH          :: Double
eINTR          :: Double
eIO            :: Double
eNXIO          :: Double
e2BIG          :: Double
eNOEXEC        :: Double
eBADF          :: Double
eCHILD         :: Double
eAGAIN         :: Double
eNOMEM         :: Double
eACCES         :: Double
eFAULT         :: Double
eNOTBLK        :: Double
eBUSY          :: Double
eEXIST         :: Double
eXDEV          :: Double
eNODEV         :: Double
eNOTDIR        :: Double
eISDIR         :: Double
eINVAL         :: Double
eNFILE         :: Double
eMFILE         :: Double
eNOTTY         :: Double
eTXTBSY        :: Double
eFBIG          :: Double
eNOSPC         :: Double
eSPIPE         :: Double
eROFS          :: Double
eMLINK         :: Double
ePIPE          :: Double
eDOM           :: Double
eRANGE         :: Double
eDEADLK        :: Double
eNAMETOOLONG   :: Double
eNOLCK         :: Double
eNOSYS         :: Double
eNOTEMPTY      :: Double
eLOOP          :: Double
eWOULDBLOCK    :: Double
eNOMSG         :: Double
eIDRM          :: Double
eCHRNG         :: Double
eL2NSYNC       :: Double
eL3HLT         :: Double
eL3RST         :: Double
eLNRNG         :: Double
eUNATCH        :: Double
eNOCSI         :: Double
eL2HLT         :: Double
eBADE          :: Double
eBADR          :: Double
eXFULL         :: Double
eNOANO         :: Double
eBADRQC        :: Double
eBADSLT        :: Double
eDEADLOCK      :: Double
eBFONT         :: Double
eNOSTR         :: Double
eNODATA        :: Double
eTIME          :: Double
eNOSR          :: Double
eNONET         :: Double
eNOPKG         :: Double
eREMOTE        :: Double
eNOLINK        :: Double
eADV           :: Double
eSRMNT         :: Double
eCOMM          :: Double
ePROTO         :: Double
eMULTIHOP      :: Double
eDOTDOT        :: Double
eBADMSG        :: Double
eOVERFLOW      :: Double
eNOTUNIQ       :: Double
eBADFD         :: Double
eREMCHG        :: Double
eLIBACC        :: Double
eLIBBAD        :: Double
eLIBSCN        :: Double
eLIBMAX        :: Double
eLIBEXEC       :: Double
eILSEQ         :: Double
eRESTART       :: Double
eSTRPIPE       :: Double
eUSERS         :: Double
eNOTSOCK       :: Double
eDESTADDRREQ   :: Double
eMSGSIZE       :: Double
ePROTOTYPE     :: Double
eNOPROTOOPT    :: Double
ePROTONOSUPPORT:: Double
eSOCKTNOSUPPORT:: Double
eOPNOTSUPP     :: Double
ePFNOSUPPORT   :: Double
eAFNOSUPPORT   :: Double
eADDRINUSE     :: Double
eADDRNOTAVAIL  :: Double
eNETDOWN       :: Double
eNETUNREACH    :: Double
eNETRESET      :: Double
eCONNABORTED   :: Double
eCONNRESET     :: Double
eNOBUFS        :: Double
eISCONN        :: Double
eNOTCONN       :: Double
eSHUTDOWN      :: Double
eTOOMANYREFS   :: Double
eTIMEDOUT      :: Double
eCONNREFUSED   :: Double
eHOSTDOWN      :: Double
eHOSTUNREACH   :: Double
eALREADY       :: Double
eINPROGRESS    :: Double
eSTALE         :: Double
eUCLEAN        :: Double
eNOTNAM        :: Double
eNAVAIL        :: Double
eISNAM         :: Double
eREMOTEIO      :: Double
eDQUOT         :: Double
eNOMEDIUM      :: Double
eMEDIUMTYPE    :: Double
