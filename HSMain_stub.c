#define IN_STG_CODE 0
#include "Rts.h"
#include "Stg.h"
#ifdef __cplusplus
extern "C" {
#endif
 
extern StgClosure HSMain_zdfhszumainzuatN_closure;
void hs_MAIN(void)
{
Capability *cap;
HaskellObj ret;
cap = rts_lock();
cap=rts_evalIO(cap,rts_apply(cap,(HaskellObj)runIO_closure,&HSMain_zdfhszumainzuatN_closure) ,&ret);
rts_checkSchedStatus("hs_MAIN",cap);
rts_unlock(cap);
}
static void stginit_export_HSMain_zdfhszumainzuatN() __attribute__((constructor));
static void stginit_export_HSMain_zdfhszumainzuatN()
{getStablePtr((StgPtr) &HSMain_zdfhszumainzuatN_closure);}
#ifdef __cplusplus
}
#endif

