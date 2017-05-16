{-# LANGUAGE JavaScriptFFI #-}

-- | An implementation of the NodeJS DNS API, as documented
--   <https://nodejs.org/api/dns.html here>.
--
--   Note that there are special factors that must be taken into account when
--   using this API; these considerations are documented
--   <https://nodejs.org/api/dns.html#dns_implementation_considerations here>.
module GHCJS.Node.DNS
  ( module GHCJS.Node.DNS -- FIXME: specific export list
  ) where

import           GHCJS.Array
import           GHCJS.Callback
import           GHCJS.Foreign
import           GHCJS.Nullable
import           GHCJS.Types

import           JavaScript.Object

import           GHCJS.Node.Error

import           Data.Coerce

--------------------------------------------------------------------------------

import           Data.Text         (Text)
import           Data.Set          (Set)

--------------------------------------------------------------------------------

-- | FIXME: doc
data DNSRecord
  = DNSRecordA     !DNSRecordA
  | DNSRecordAAAA  !DNSRecordAAAA
  | DNSRecordCNAME !DNSRecordCNAME
  | DNSRecordMX    !DNSRecordMX
  | DNSRecordNAPTR !DNSRecordNAPTR
  | DNSRecordNS    !DNSRecordNS
  | DNSRecordPTR   !DNSRecordPTR
  | DNSRecordSOA   !DNSRecordSOA
  | DNSRecordSRV   !DNSRecordSRV
  | DNSRecordTXT   !DNSRecordTXT

-- | FIXME: doc
data DNSRecordType
  = DNSRecordTypeA
  | DNSRecordTypeAAAA
  | DNSRecordTypeCNAME
  | DNSRecordTypeMX
  | DNSRecordTypeNAPTR
  | DNSRecordTypeNS
  | DNSRecordTypePTR
  | DNSRecordTypeSOA
  | DNSRecordTypeSRV
  | DNSRecordTypeTXT

-- | FIXME: doc
data DNSRecordA
  = MkDNSRecordA
    { _DNSRecordA_address :: !Address
    , _DNSRecordA_ttl     :: !Int
    }

-- | FIXME: doc
data DNSRecordAAAA
  = MkDNSRecordAAAA
    { _DNSRecordAAAA_address :: !Address
    , _DNSRecordAAAA_ttl     :: !Int
    }

-- | FIXME: doc
data DNSRecordCNAME
  = MkDNSRecordCNAME
    { _DNSRecordCNAME_hostname :: !HostName
    }

-- | FIXME: doc
toDNSRecordCNAME :: RecordCNAME -> DNSRecordCNAME
toDNSRecordCNAME (MkRecordCNAME hostname) = MkDNSRecordCNAME hostname

-- | FIXME: doc
data DNSRecordMX
  = MkDNSRecordMX
    { _DNSRecordMX_priority :: !Int
    , _DNSRecordMX_exchange :: !HostName
    }

-- | An NAPTR record flag as defined in
--   <https://www.ietf.org/rfc/rfc2915.txt IETF RFC 2915>.
data NAPTRFlag
  = NAPTRFlagS
  | NAPTRFlagA
  | NAPTRFlagU
  | NAPTRFlagP

-- | A regular expression for NAPTR records as defined in
--   <https://www.ietf.org/rfc/rfc2915.txt IETF RFC 2915>.
--
--   Grammar:
--     subst_expr   = delim-char  ere  delim-char  repl  delim-char  *flags
--     delim-char   = "/" / "!" / ... <Any non-digit or non-flag character
--                    other than backslash '\'. All occurences of a delim_char
--                    in a subst_expr must be the same character.>
--     ere          = POSIX Extended Regular Expression
--     repl         = 1 * ( OCTET /  backref )
--     backref      = "\" 1POS_DIGIT
--     flags        = "i"
--     POS_DIGIT    = %x31-39                 ; 0 is not an allowed backref
--
--   FIXME: improve type safety
type NAPTRRegex = Text

-- | FIXME: doc
data DNSRecordNAPTR
  = MkDNSRecordNAPTR
    { _DNSRecordNAPTR_flags       :: !(Set NAPTRFlag)
    , _DNSRecordNAPTR_service     :: !Service
    , _DNSRecordNAPTR_regexp      :: !NAPTRRegex
    , _DNSRecordNAPTR_replacement :: !Text
    , _DNSRecordNAPTR_order       :: !Int
    , _DNSRecordNAPTR_preference  :: !Int
    }

-- | FIXME: doc
data DNSRecordNS
  = MkDNSRecordNS
    { _DNSRecordNS_hostname :: !HostName
    }

-- | FIXME: doc
toDNSRecordNS :: RecordNS -> DNSRecordNS
toDNSRecordNS (MkRecordNS hostname) = MkDNSRecordNS hostname

-- | FIXME: doc
data DNSRecordPTR
  = MkDNSRecordPTR
    { _DNSRecordPTR_hostname :: !HostName
    }

-- | FIXME: doc
toDNSRecordPTR :: RecordPTR -> DNSRecordPTR
toDNSRecordPTR (MkRecordPTR hostname) = MkDNSRecordPTR hostname

-- | FIXME: doc
data DNSRecordSOA
  = MkDNSRecordSOA
    { _DNSRecordSOA_nsname     :: !HostName
    , _DNSRecordSOA_hostmaster :: !HostName
    , _DNSRecordSOA_serial     :: !Int
    , _DNSRecordSOA_refresh    :: !Int
    , _DNSRecordSOA_retry      :: !Int
    , _DNSRecordSOA_expire     :: !Int
    , _DNSRecordSOA_minttl     :: !Int
    }

-- | FIXME: doc
data DNSRecordSRV
  = MkDNSRecordSRV
    { _DNSRecordSRV_priority :: !Int
    , _DNSRecordSRV_weight   :: !Int
    , _DNSRecordSRV_port     :: !Int
    , _DNSRecordSRV_name     :: !HostName
    }

-- | FIXME: doc
data DNSRecordTXT
  = MkDNSRecordTXT
    { _DNSRecordTXT_chunks :: ![Text]
    }

--------------------------------------------------------------------------------

-- | FIXME: doc
newtype HostName
  = MkHostName JSString

-- | FIXME: doc
fromHostName :: HostName -> Text
fromHostName = undefined -- FIXME: implement

-- | FIXME: doc
toHostName :: Text -> HostName
toHostName = undefined -- FIXME: implement

--------------------------------------------------------------------------------

-- | FIXME: doc
newtype Address
  = MkAddress JSString

-- | FIXME: doc
fromAddress :: Address -> Text
fromAddress = undefined -- FIXME: implement

-- | FIXME: doc
toAddress :: Text -> Address
toAddress = undefined -- FIXME: implement

--------------------------------------------------------------------------------

-- | FIXME: doc
newtype Service
  = MkService JSString

-- | FIXME: doc
fromService :: Service -> Text
fromService = undefined -- FIXME: implement

-- | FIXME: doc
toService :: Text -> Service
toService = undefined -- FIXME: implement

--------------------------------------------------------------------------------

-- | FIXME: doc
type Port = Int

--------------------------------------------------------------------------------

-- | FIXME: doc
newtype Hints
  = MkHints JSVal

-- | FIXME: doc
foreign import javascript unsafe
  "$r = dns.ADDRCONFIG;"
  hintADDRCONFIG :: Hints

-- | FIXME: doc
foreign import javascript unsafe
  "$r = dns.V4MAPPED;"
  hintV4MAPPED :: Hints

-- | FIXME: doc
foreign import javascript unsafe
  "$r = (($1) | ($2));"
  hintCombine :: Hints -> Hints -> Hints

--------------------------------------------------------------------------------

-- | The type of IP address families (i.e.: IPv4 versus IPv6).
newtype Family
  = MkFamily JSVal

-- | The address family representing IPv4 addresses.
foreign import javascript unsafe "$r = 4;"
  familyIPv4 :: Family

-- | The address family representing IPv6 addresses.
foreign import javascript unsafe "$r = 6;"
  familyIPv6 :: Family

-- -- | Parse an arbitrary 'JSVal' into a 'Family'.
-- parseFamily :: JSVal -> Maybe Family
-- parseFamily val = if val == ip4
--                   then Just familyIPv4
--                   else if val == ip6
--                        then Just familyIPv6
--                        else Nothing
--   where
--     ip4 = (pToJSVal familyIPv4) :: JSVal
--     ip6 = (pToJSVal familyIPv6) :: JSVal
-- FIXME: reimplement properly

--------------------------------------------------------------------------------

-- | FIXME: doc
newtype Resolved
  = MkResolved Object

-- | FIXME: doc
fromResolved :: Resolved -> (Address, Family)
fromResolved = undefined -- FIXME: implement

-- | FIXME: doc
toResolved :: (Address, Family) -> Resolved
toResolved = undefined -- FIXME: implement

--------------------------------------------------------------------------------

-- | FIXME: doc
--   @{address: "1.2.3.4", ttl: 60}@
newtype RecordA
  = MkRecordA Object

-- | FIXME: doc
--   @{address: "1.2.3.4", ttl: 60}@
newtype RecordAAAA
  = MkRecordAAAA Object

-- | FIXME: doc
--   @"bar.example.com"@
newtype RecordCNAME
  = MkRecordCNAME HostName

-- | FIXME: doc
--   @{priority: 10, exchange: "mx.example.com"}@
newtype RecordMX
  = MkRecordMX Object

-- | FIXME: doc
--   {
--     flags: "s",
--     service: "SIP+D2U",
--     regexp: "",
--     replacement: "_sip._udp.example.com",
--     order: 30,
--     preference: 100
--   }
newtype RecordNAPTR
  = MkRecordNAPTR Object

-- | FIXME: doc
--   @"ns1.example.com"@
newtype RecordNS
  = MkRecordNS HostName

-- | FIXME: doc
--   @"mail.example.com"@
newtype RecordPTR
  = MkRecordPTR HostName

-- | FIXME: doc
--   {
--     nsname: "ns.example.com",
--     hostmaster: "root.example.com",
--     serial: 2013101809,
--     refresh: 10000,
--     retry: 2400,
--     expire: 604800,
--     minttl: 3600
--   }
newtype RecordSOA
  = MkRecordSOA Object

-- | FIXME: doc
--   {
--     priority: 10,
--     weight: 5,
--     port: 21223,
--     name: "service.example.com"
--   }
newtype RecordSRV
  = MkRecordSRV Object

-- | FIXME: doc
--   @["v=spf1 ip4:0.0.0.0 ", "~all"]@
newtype RecordTXT
  = MkRecordTXT (Array JSString)

--------------------------------------------------------------------------------

-- | Returns an array of all the IP addresses, encoded as strings, that are
--   being used for name resolution.
foreign import javascript safe
  "$r = dns.getServers();"
  unsafeGetServers :: IO (Array Address)

-- | Resolves the given hostname (e.g.: @"nodejs.org"@) into the first found
--   A (IPv4) or AAAA (IPv6) record.
--
--   This function is equivalent to @dns.lookup(…, {…, all: false}, …)@.
--
--   In the returned value, either the 'Error' object is defined or the rest of
--   the values are.
foreign import javascript interruptible
  "dns.lookup($1, {family: $2, hints: $3, all: false}, function(e, a, f) { $c(e, a, f); });"
  unsafeLookup :: HostName
               -> Family
               -> Hints
               -> IO (Nullable Error, Nullable Address, Nullable Family)

-- | Resolves the given hostname (e.g.: @"nodejs.org"@) into the first found
--   A (IPv4) or AAAA (IPv6) record.
--
--   This function is equivalent to @dns.lookup(…, {…, all: true}, …)@.
foreign import javascript interruptible
  "dns.lookup($1, {family: $2, hints: $3, all: true}, function(e, r) { $c(e, r); });"
  unsafeLookupAll :: HostName
                  -> Family
                  -> Hints
                  -> IO (Nullable Error, Nullable (Array Resolved))

-- | Resolves the given address and port into a hostname and service using the
--   operating system's underlying @getnameinfo@ implementation.
--
--   In the returned value, either the 'Error' object is defined or the rest of
--   the values are.
foreign import javascript interruptible
  "dns.lookupService($1, $2, function(e, h, s) { $c(e, h, s); });"
  unsafeLookupService :: HostName
                      -> Port
                      -> IO (Nullable Error, Nullable HostName, Nullable Service)

-- | Uses the DNS protocol to resolve the A records for the given hostname.
--
--   In the returned value, either the 'Error' object is defined or the rest of
--   the values are.
foreign import javascript interruptible
  "dns.resolve4($1, {ttl: true}, function(e, a) { $c(e, a); });"
  unsafeResolveA :: HostName
                 -> IO (Nullable Error, Nullable (Array RecordA))

-- | Uses the DNS protocol to resolve the AAAA records for the given hostname.
--
--   In the returned value, either the 'Error' object is defined or the rest of
--   the values are.
foreign import javascript interruptible
  "dns.resolve6($1, {ttl: true}, function(e, a) { $c(e, a); });"
  unsafeResolveAAAA :: HostName
                    -> IO (Nullable Error, Nullable (Array RecordAAAA))

-- | Uses the DNS protocol to resolve the CNAME records for the given hostname.
--
--   In the returned value, either the 'Error' object is defined or the rest of
--   the values are.
foreign import javascript interruptible
  "dns.resolveCname($1, function(e, a) { $c(e, a); });"
  unsafeResolveCNAME :: HostName
                     -> IO (Nullable Error, Nullable (Array RecordCNAME))

-- | Uses the DNS protocol to resolve the MX records for the given hostname.
--
--   In the returned value, either the 'Error' object is defined or the rest of
--   the values are.
foreign import javascript interruptible
  "dns.resolveMx($1, function(e, a) { $c(e, a); });"
  unsafeResolveMX :: HostName
                  -> IO (Nullable Error, Nullable (Array RecordMX))

-- | Uses the DNS protocol to resolve the NAPTR records for the given hostname.
--
--   In the returned value, either the 'Error' object is defined or the rest of
--   the values are.
foreign import javascript interruptible
  "dns.resolveNaptr($1, function(e, a) { $c(e, a); });"
  unsafeResolveNAPTR :: HostName
                     -> IO (Nullable Error, Nullable (Array RecordNAPTR))

-- | Uses the DNS protocol to resolve the NS records for the given hostname.
--
--   In the returned value, either the 'Error' object is defined or the rest of
--   the values are.
foreign import javascript interruptible
  "dns.resolveNs($1, function(e, a) { $c(e, a); });"
  unsafeResolveNS :: HostName
                     -> IO (Nullable Error, Nullable (Array RecordNS))

-- | Uses the DNS protocol to resolve the PTR records for the given hostname.
--
--   In the returned value, either the 'Error' object is defined or the rest of
--   the values are.
foreign import javascript interruptible
  "dns.resolvePtr($1, function(e, a) { $c(e, a); });"
  unsafeResolvePTR :: HostName
                   -> IO (Nullable Error, Nullable (Array RecordPTR))

-- | Uses the DNS protocol to resolve the SOA records for the given hostname.
--
--   In the returned value, either the 'Error' object is defined or the rest of
--   the values are.
foreign import javascript interruptible
  "dns.resolveSoa($1, function(e, a) { $c(e, a); });"
  unsafeResolveSOA :: HostName
                   -> IO (Nullable Error, Nullable (Array RecordSOA))

-- | Uses the DNS protocol to resolve the SRV records for the given hostname.
--
--   In the returned value, either the 'Error' object is defined or the rest of
--   the values are.
foreign import javascript interruptible
  "dns.resolveSrv($1, function(e, a) { $c(e, a); });"
  unsafeResolveSRV :: HostName
                   -> IO (Nullable Error, Nullable (Array RecordSRV))

-- | Uses the DNS protocol to resolve the TXT records for the given hostname.
--
--   In the returned value, either the 'Error' object is defined or the rest of
--   the values are.
foreign import javascript interruptible
  "dns.resolveTxt($1, function(e, a) { $c(e, a); });"
  unsafeResolveTXT :: HostName
                   -> IO (Nullable Error, Nullable (Array RecordTXT))

-- | Performs a reverse DNS query that resolves an IPv4 or IPv6 address to an
--   array of hostnames.
--
--   The 'Error' object contains a field named @code@ of type 'DNSError'.
--
--   In the returned value, either the 'Error' object is defined or the rest of
--   the values are.
foreign import javascript interruptible
  "dns.reverse($1, function(e, h) { $c(e, h); });"
  unsafeReverse :: Address
                -> IO (Nullable Error, Nullable (Array HostName))

-- | Sets the IP addresses of the servers to be used when resolving.
--   The @servers@ argument is an array of IPv4 or IPv6 addresses.
--
--   If a port is specified on the address, it will be removed.
--
--   An error will be thrown if an invalid address is provided.
--
--   This function must not be called while a DNS query is in progress.
foreign import javascript safe
  "dns.setServers($1);"
  unsafeSetServers :: Array Address
                   -> IO ()

--------------------------------------------------------------------------------

-- | A DNS error code.
newtype DNSError
  = MkDNSError JSString

-- | DNS error: "DNS server returned answer with no data."
foreign import javascript unsafe "$r = dns.NODATA;"
  dnsNODATA :: DNSError

-- | DNS error: "DNS server claims query was misformatted."
foreign import javascript unsafe "$r = dns.FORMERR;"
  dnsFORMERR :: DNSError

-- | DNS error: "DNS server returned general failure."
foreign import javascript unsafe "$r = dns.SERVFAIL;"
  dnsSERVFAIL :: DNSError

-- | DNS error: "Domain name not found."
foreign import javascript unsafe "$r = dns.NOTFOUND;"
  dnsNOTFOUND :: DNSError

-- | DNS error: "DNS server does not implement this operation."
foreign import javascript unsafe "$r = dns.NOTIMP;"
  dnsNOTIMP :: DNSError

-- | DNS error: "DNS server refused query."
foreign import javascript unsafe "$r = dns.REFUSED;"
  dnsREFUSED :: DNSError

-- | DNS error: "Misformatted DNS query."
foreign import javascript unsafe "$r = dns.BADQUERY;"
  dnsBADQUERY :: DNSError

-- | DNS error: "Misformatted hostname."
foreign import javascript unsafe "$r = dns.BADNAME;"
  dnsBADNAME :: DNSError

-- | DNS error: "Unsupported address family."
foreign import javascript unsafe "$r = dns.BADFAMILY;"
  dnsBADFAMILY :: DNSError

-- | DNS error: "Misformatted DNS reply."
foreign import javascript unsafe "$r = dns.BADRESP;"
  dnsBADRESP :: DNSError

-- | DNS error: "Could not contact DNS servers."
foreign import javascript unsafe "$r = dns.CONNREFUSED;"
  dnsCONNREFUSED :: DNSError

-- | DNS error: "Timeout while contacting DNS servers."
foreign import javascript unsafe "$r = dns.TIMEOUT;"
  dnsTIMEOUT :: DNSError

-- | DNS error: "End of file."
foreign import javascript unsafe "$r = dns.EOF;"
  dnsEOF :: DNSError

-- | DNS error: "Error reading file."
foreign import javascript unsafe "$r = dns.FILE;"
  dnsFILE :: DNSError

-- | DNS error: "Out of memory."
foreign import javascript unsafe "$r = dns.NOMEM;"
  dnsNOMEM :: DNSError

-- | DNS error: "Channel is being destroyed."
foreign import javascript unsafe "$r = dns.DESTRUCTION;"
  dnsDESTRUCTION :: DNSError

-- | DNS error: "Misformatted string."
foreign import javascript unsafe "$r = dns.BADSTR;"
  dnsBADSTR :: DNSError

-- | DNS error: "Illegal flags specified."
foreign import javascript unsafe "$r = dns.BADFLAGS;"
  dnsBADFLAGS :: DNSError

-- | DNS error: "Given hostname is not numeric."
foreign import javascript unsafe "$r = dns.NONAME;"
  dnsNONAME :: DNSError

-- | DNS error: "Illegal hints flags specified."
foreign import javascript unsafe "$r = dns.BADHINTS;"
  dnsBADHINTS :: DNSError

-- | DNS error: "@c-ares@ initialization not yet performed."
foreign import javascript unsafe "$r = dns.NOTINITIALIZED;"
  dnsNOTINITIALIZED :: DNSError

-- | DNS error: "Error loading @iphlpapi.dll@."
foreign import javascript unsafe "$r = dns.LOADIPHLPAPI;"
  dnsLOADIPHLPAPI :: DNSError

-- | DNS error: "Could not find @GetNetworkParams@ function."
foreign import javascript unsafe "$r = dns.ADDRGETNETWORKPARAMS;"
  dnsADDRGETNETWORKPARAMS :: DNSError

-- | DNS error: "DNS query cancelled."
foreign import javascript unsafe "$r = dns.CANCELLED;"
  dnsCANCELLED :: DNSError

--------------------------------------------------------------------------------
