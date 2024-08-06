#include <stdint.h>

typedef struct _Nvmber Nvmber;

typedef enum NvmberResult {
    NVMBER_OK,
    NVMBER_TOO_LARGE,
    NVMBER_MALFORMED,
    NVMBER_INVALID_STR,
    NVMBER_IS_NULL,
} NvmberResult;

NvmberResult nvmber_new(const char *str, Nvmber **out);

int nvmber_get_int(Nvmber *nvmber);

Nvmber* nvmber_sum(Nvmber *a, Nvmber* b);

void nvmber_free(Nvmber *nvmber);
