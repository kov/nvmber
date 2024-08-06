#include <assert.h>
#include <stdio.h>

#include "nvmber.h"

int main(int argc, char** argv) {
  Nvmber* nvmber = NULL;
  if (nvmber_new("XVII", &nvmber) != NVMBER_OK) {
    fprintf(stderr, "Failed to create nvmber.");
    return 1;
  }
  
  Nvmber* decem = NULL;
  if (nvmber_new("X", &decem) != NVMBER_OK) {
    fprintf(stderr, "Failed to create decem.");
    return 1;
  }

  Nvmber* svm = nvmber_sum(nvmber, decem);

  printf("nvmber: %d devem: %d svm: %d\n",
         nvmber_get_int(nvmber),
         nvmber_get_int(decem),
         nvmber_get_int(svm));

  Nvmber* fail = NULL;
  NvmberResult r = nvmber_new("XVV", &fail);
  assert(r == NVMBER_MALFORMED);

  return 0;
}
