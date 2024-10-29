#include "nix_api_util.h"
#include "nix_api_store.h"
#include "nix_api_expr.h"
#include "nix_api_value.h"

void wrapper_nix_value_decref(nix_value *value) {
  nix_value_decref(NULL, value);
}

void (*wrapper_get_nix_value_decref(void)) (nix_value *value) {
  return &wrapper_nix_value_decref;
}

void (*wrapper_get_nix_store_free(void)) (Store *store) {
  return &nix_store_free;
}

void (*wrapper_get_nix_c_context_free(void)) (nix_c_context *context) {
  return &nix_c_context_free;
}

void (*wrapper_get_nix_state_free(void)) (EvalState *state) {
  return &nix_state_free;
}

void (*wrapper_get_nix_store_path_free(void)) (StorePath *p) {
  return &nix_store_path_free;
}
