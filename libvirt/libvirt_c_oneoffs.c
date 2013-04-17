/* OCaml bindings for libvirt.
 * (C) Copyright 2007 Richard W.M. Jones, Red Hat Inc.
 * http://libvirt.org/
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

/* Please read libvirt/README file. */

/*----------------------------------------------------------------------*/

CAMLprim value
ocaml_libvirt_get_version (value driverv, value unit)
{
  CAMLparam2 (driverv, unit);
  CAMLlocal1 (rv);
  const char *driver = Optstring_val (driverv);
  unsigned long libVer, typeVer = 0, *typeVer_ptr;
  int r;

  typeVer_ptr = driver ? &typeVer : NULL;
  NONBLOCKING (r = virGetVersion (&libVer, driver, typeVer_ptr));
  CHECK_ERROR (r == -1, NULL, "virGetVersion");

  rv = caml_alloc_tuple (2);
  Store_field (rv, 0, Val_int (libVer));
  Store_field (rv, 1, Val_int (typeVer));
  CAMLreturn (rv);
}

/*----------------------------------------------------------------------*/

/* Connection object. */

CAMLprim value
ocaml_libvirt_connect_open (value namev, value unit)
{
  CAMLparam2 (namev, unit);
  CAMLlocal1 (rv);
  const char *name = Optstring_val (namev);
  virConnectPtr conn;

  NONBLOCKING (conn = virConnectOpen (name));
  CHECK_ERROR (!conn, NULL, "virConnectOpen");

  rv = Val_connect (conn);

  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_connect_open_readonly (value namev, value unit)
{
  CAMLparam2 (namev, unit);
  CAMLlocal1 (rv);
  const char *name = Optstring_val (namev);
  virConnectPtr conn;

  NONBLOCKING (conn = virConnectOpenReadOnly (name));
  CHECK_ERROR (!conn, NULL, "virConnectOpen");

  rv = Val_connect (conn);

  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_connect_get_version (value connv)
{
  CAMLparam1 (connv);
  virConnectPtr conn = Connect_val (connv);
  unsigned long hvVer;
  int r;

  NONBLOCKING (r = virConnectGetVersion (conn, &hvVer));
  CHECK_ERROR (r == -1, conn, "virConnectGetVersion");

  CAMLreturn (Val_int (hvVer));
}

CAMLprim value
ocaml_libvirt_connect_get_max_vcpus (value connv, value typev)
{
  CAMLparam2 (connv, typev);
  virConnectPtr conn = Connect_val (connv);
  const char *type = Optstring_val (typev);
  int r;

  NONBLOCKING (r = virConnectGetMaxVcpus (conn, type));
  CHECK_ERROR (r == -1, conn, "virConnectGetMaxVcpus");

  CAMLreturn (Val_int (r));
}

CAMLprim value
ocaml_libvirt_connect_get_node_info (value connv)
{
  CAMLparam1 (connv);
  CAMLlocal2 (rv, v);
  virConnectPtr conn = Connect_val (connv);
  virNodeInfo info;
  int r;

  NONBLOCKING (r = virNodeGetInfo (conn, &info));
  CHECK_ERROR (r == -1, conn, "virNodeGetInfo");

  rv = caml_alloc (8, 0);
  v = caml_copy_string (info.model); Store_field (rv, 0, v);
  v = caml_copy_int64 (info.memory); Store_field (rv, 1, v);
  Store_field (rv, 2, Val_int (info.cpus));
  Store_field (rv, 3, Val_int (info.mhz));
  Store_field (rv, 4, Val_int (info.nodes));
  Store_field (rv, 5, Val_int (info.sockets));
  Store_field (rv, 6, Val_int (info.cores));
  Store_field (rv, 7, Val_int (info.threads));

  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_connect_node_get_free_memory (value connv)
{
  CAMLparam1 (connv);
  CAMLlocal1 (rv);
  virConnectPtr conn = Connect_val (connv);
  unsigned long long r;

  NONBLOCKING (r = virNodeGetFreeMemory (conn));
  CHECK_ERROR (r == 0, conn, "virNodeGetFreeMemory");

  rv = caml_copy_int64 ((int64) r);
  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_connect_node_get_cells_free_memory (value connv,
						  value startv, value maxv)
{
  CAMLparam3 (connv, startv, maxv);
  CAMLlocal2 (rv, iv);
  virConnectPtr conn = Connect_val (connv);
  int start = Int_val (startv);
  int max = Int_val (maxv);
  int r, i;
  unsigned long long freemems[max];

  NONBLOCKING (r = virNodeGetCellsFreeMemory (conn, freemems, start, max));
  CHECK_ERROR (r == -1, conn, "virNodeGetCellsFreeMemory");

  rv = caml_alloc (r, 0);
  for (i = 0; i < r; ++i) {
    iv = caml_copy_int64 ((int64) freemems[i]);
    Store_field (rv, i, iv);
  }

  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_connect_set_keep_alive(value connv,
				     value intervalv, value countv)
{
  CAMLparam3 (connv, intervalv, countv);
  virConnectPtr conn = Connect_val(connv);
  int interval = Int_val(intervalv);
  unsigned int count = Int_val(countv);
  int r;

  NONBLOCKING(r = virConnectSetKeepAlive(conn, interval, count));
  CHECK_ERROR (r == -1, conn, "virConnectSetKeepAlive");

  CAMLreturn(Val_unit);
}


CAMLprim value
ocaml_libvirt_domain_get_id (value domv)
{
  CAMLparam1 (domv);
  virDomainPtr dom = Domain_val (domv);
  /*virConnectPtr conn = Connect_domv (domv);*/
  unsigned int r;

  NONBLOCKING (r = virDomainGetID (dom));
  /* In theory this could return -1 on error, but in practice
   * libvirt never does this unless you call it with a corrupted
   * or NULL dom object.  So ignore errors here.
   */

  CAMLreturn (Val_int ((int) r));
}

CAMLprim value
ocaml_libvirt_domain_get_max_memory (value domv)
{
  CAMLparam1 (domv);
  CAMLlocal1 (rv);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  unsigned long r;

  NONBLOCKING (r = virDomainGetMaxMemory (dom));
  CHECK_ERROR (r == 0 /* [sic] */, conn, "virDomainGetMaxMemory");

  rv = caml_copy_int64 (r);
  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_domain_set_max_memory (value domv, value memv)
{
  CAMLparam2 (domv, memv);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  unsigned long mem = Int64_val (memv);
  int r;

  NONBLOCKING (r = virDomainSetMaxMemory (dom, mem));
  CHECK_ERROR (r == -1, conn, "virDomainSetMaxMemory");

  CAMLreturn (Val_unit);
}

CAMLprim value
ocaml_libvirt_domain_set_memory (value domv, value memv)
{
  CAMLparam2 (domv, memv);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  unsigned long mem = Int64_val (memv);
  int r;

  NONBLOCKING (r = virDomainSetMemory (dom, mem));
  CHECK_ERROR (r == -1, conn, "virDomainSetMemory");

  CAMLreturn (Val_unit);
}

CAMLprim value
ocaml_libvirt_domain_get_info (value domv)
{
  CAMLparam1 (domv);
  CAMLlocal2 (rv, v);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  virDomainInfo info;
  int r;

  NONBLOCKING (r = virDomainGetInfo (dom, &info));
  CHECK_ERROR (r == -1, conn, "virDomainGetInfo");

  rv = caml_alloc (5, 0);
  Store_field (rv, 0, Val_int (info.state)); // These flags are compatible.
  v = caml_copy_int64 (info.maxMem); Store_field (rv, 1, v);
  v = caml_copy_int64 (info.memory); Store_field (rv, 2, v);
  Store_field (rv, 3, Val_int (info.nrVirtCpu));
  v = caml_copy_int64 (info.cpuTime); Store_field (rv, 4, v);

  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_domain_get_scheduler_type (value domv)
{
  CAMLparam1 (domv);
  CAMLlocal2 (rv, strv);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  char *r;
  int nparams;

  NONBLOCKING (r = virDomainGetSchedulerType (dom, &nparams));
  CHECK_ERROR (!r, conn, "virDomainGetSchedulerType");

  rv = caml_alloc_tuple (2);
  strv = caml_copy_string (r); Store_field (rv, 0, strv);
  free (r);
  Store_field (rv, 1, nparams);
  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_domain_get_scheduler_parameters (value domv, value nparamsv)
{
  CAMLparam2 (domv, nparamsv);
  CAMLlocal4 (rv, v, v2, v3);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  int nparams = Int_val (nparamsv);
  virSchedParameter params[nparams];
  int r, i;

  NONBLOCKING (r = virDomainGetSchedulerParameters (dom, params, &nparams));
  CHECK_ERROR (r == -1, conn, "virDomainGetSchedulerParameters");

  rv = caml_alloc (nparams, 0);
  for (i = 0; i < nparams; ++i) {
    v = caml_alloc_tuple (2); Store_field (rv, i, v);
    v2 = caml_copy_string (params[i].field); Store_field (v, 0, v2);
    switch (params[i].type) {
    case VIR_DOMAIN_SCHED_FIELD_INT:
      v2 = caml_alloc (1, 0);
      v3 = caml_copy_int32 (params[i].value.i); Store_field (v2, 0, v3);
      break;
    case VIR_DOMAIN_SCHED_FIELD_UINT:
      v2 = caml_alloc (1, 1);
      v3 = caml_copy_int32 (params[i].value.ui); Store_field (v2, 0, v3);
      break;
    case VIR_DOMAIN_SCHED_FIELD_LLONG:
      v2 = caml_alloc (1, 2);
      v3 = caml_copy_int64 (params[i].value.l); Store_field (v2, 0, v3);
      break;
    case VIR_DOMAIN_SCHED_FIELD_ULLONG:
      v2 = caml_alloc (1, 3);
      v3 = caml_copy_int64 (params[i].value.ul); Store_field (v2, 0, v3);
      break;
    case VIR_DOMAIN_SCHED_FIELD_DOUBLE:
      v2 = caml_alloc (1, 4);
      v3 = caml_copy_double (params[i].value.d); Store_field (v2, 0, v3);
      break;
    case VIR_DOMAIN_SCHED_FIELD_BOOLEAN:
      v2 = caml_alloc (1, 5);
      Store_field (v2, 0, Val_int (params[i].value.b));
      break;
    default:
      caml_failwith ((char *)__FUNCTION__);
    }
    Store_field (v, 1, v2);
  }
  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_domain_set_scheduler_parameters (value domv, value paramsv)
{
  CAMLparam2 (domv, paramsv);
  CAMLlocal1 (v);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  int nparams = Wosize_val (paramsv);
  virSchedParameter params[nparams];
  int r, i;
  char *name;

  for (i = 0; i < nparams; ++i) {
    v = Field (paramsv, i);	/* Points to the two-element tuple. */
    name = String_val (Field (v, 0));
    strncpy (params[i].field, name, VIR_DOMAIN_SCHED_FIELD_LENGTH);
    params[i].field[VIR_DOMAIN_SCHED_FIELD_LENGTH-1] = '\0';
    v = Field (v, 1);		/* Points to the sched_param_value block. */
    switch (Tag_val (v)) {
    case 0:
      params[i].type = VIR_DOMAIN_SCHED_FIELD_INT;
      params[i].value.i = Int32_val (Field (v, 0));
      break;
    case 1:
      params[i].type = VIR_DOMAIN_SCHED_FIELD_UINT;
      params[i].value.ui = Int32_val (Field (v, 0));
      break;
    case 2:
      params[i].type = VIR_DOMAIN_SCHED_FIELD_LLONG;
      params[i].value.l = Int64_val (Field (v, 0));
      break;
    case 3:
      params[i].type = VIR_DOMAIN_SCHED_FIELD_ULLONG;
      params[i].value.ul = Int64_val (Field (v, 0));
      break;
    case 4:
      params[i].type = VIR_DOMAIN_SCHED_FIELD_DOUBLE;
      params[i].value.d = Double_val (Field (v, 0));
      break;
    case 5:
      params[i].type = VIR_DOMAIN_SCHED_FIELD_BOOLEAN;
      params[i].value.b = Int_val (Field (v, 0));
      break;
    default:
      caml_failwith ((char *)__FUNCTION__);
    }
  }

  NONBLOCKING (r = virDomainSetSchedulerParameters (dom, params, nparams));
  CHECK_ERROR (r == -1, conn, "virDomainSetSchedulerParameters");

  CAMLreturn (Val_unit);
}

CAMLprim value
ocaml_libvirt_domain_set_vcpus (value domv, value nvcpusv)
{
  CAMLparam2 (domv, nvcpusv);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  int r, nvcpus = Int_val (nvcpusv);

  NONBLOCKING (r = virDomainSetVcpus (dom, nvcpus));
  CHECK_ERROR (r == -1, conn, "virDomainSetVcpus");

  CAMLreturn (Val_unit);
}

CAMLprim value
ocaml_libvirt_domain_pin_vcpu (value domv, value vcpuv, value cpumapv)
{
  CAMLparam3 (domv, vcpuv, cpumapv);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  int maplen = caml_string_length (cpumapv);
  unsigned char *cpumap = (unsigned char *) String_val (cpumapv);
  int vcpu = Int_val (vcpuv);
  int r;

  NONBLOCKING (r = virDomainPinVcpu (dom, vcpu, cpumap, maplen));
  CHECK_ERROR (r == -1, conn, "virDomainPinVcpu");

  CAMLreturn (Val_unit);
}

CAMLprim value
ocaml_libvirt_domain_get_vcpus (value domv, value maxinfov, value maplenv)
{
  CAMLparam3 (domv, maxinfov, maplenv);
  CAMLlocal5 (rv, infov, strv, v, v2);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  int maxinfo = Int_val (maxinfov);
  int maplen = Int_val (maplenv);
  virVcpuInfo info[maxinfo];
  unsigned char cpumaps[maxinfo * maplen];
  int r, i;

  memset (info, 0, sizeof (virVcpuInfo) * maxinfo);
  memset (cpumaps, 0, maxinfo * maplen);

  NONBLOCKING (r = virDomainGetVcpus (dom, info, maxinfo, cpumaps, maplen));
  CHECK_ERROR (r == -1, conn, "virDomainPinVcpu");

  /* Copy the virVcpuInfo structures. */
  infov = caml_alloc (maxinfo, 0);
  for (i = 0; i < maxinfo; ++i) {
    v2 = caml_alloc (4, 0); Store_field (infov, i, v2);
    Store_field (v2, 0, Val_int (info[i].number));
    Store_field (v2, 1, Val_int (info[i].state));
    v = caml_copy_int64 (info[i].cpuTime); Store_field (v2, 2, v);
    Store_field (v2, 3, Val_int (info[i].cpu));
  }

  /* Copy the bitmap. */
  strv = caml_alloc_string (maxinfo * maplen);
  memcpy (String_val (strv), cpumaps, maxinfo * maplen);

  /* Allocate the tuple and return it. */
  rv = caml_alloc_tuple (3);
  Store_field (rv, 0, Val_int (r)); /* number of CPUs. */
  Store_field (rv, 1, infov);
  Store_field (rv, 2, strv);

  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_domain_get_cpu_stats (value domv)
{
  CAMLparam1 (domv);
  CAMLlocal5 (cpustats, param_head, param_node, typed_param, typed_param_value);
  CAMLlocal1 (v);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  virTypedParameterPtr params;
  int r, cpu, ncpus, nparams, i, j, pos;
  int nr_pcpus;

  /* get number of pcpus */
  NONBLOCKING (nr_pcpus = virDomainGetCPUStats(dom, NULL, 0, 0, 0, 0));
  CHECK_ERROR (nr_pcpus < 0, conn, "virDomainGetCPUStats");

  /* get percpu information */
  NONBLOCKING (nparams = virDomainGetCPUStats(dom, NULL, 0, 0, 1, 0));
  CHECK_ERROR (nparams < 0, conn, "virDomainGetCPUStats");

  if ((params = malloc(sizeof(*params) * nparams * 128)) == NULL)
    caml_failwith ("virDomainGetCPUStats: malloc");

  cpustats = caml_alloc (nr_pcpus, 0); /* cpustats: array of params(list of typed_param) */
  cpu = 0;
  while (cpu < nr_pcpus) {
    ncpus = nr_pcpus - cpu > 128 ? 128 : nr_pcpus - cpu;

    NONBLOCKING (r = virDomainGetCPUStats(dom, params, nparams, cpu, ncpus, 0));
    CHECK_ERROR (r < 0, conn, "virDomainGetCPUStats");

    for (i = 0; i < ncpus; i++) {
      /* list of typed_param: single linked list of param_nodes */
      param_head = Val_emptylist; /* param_head: the head param_node of list of typed_param */

      if (params[i * nparams].type == 0) {
        Store_field(cpustats, cpu + i, param_head);
        continue;
      }

      for (j = r - 1; j >= 0; j--) {
        pos = i * nparams + j;
          if (params[pos].type == 0)
            continue;

        param_node = caml_alloc(2, 0); /* param_node: typed_param, next param_node */
        Store_field(param_node, 1, param_head);
        param_head = param_node;

        typed_param = caml_alloc(2, 0); /* typed_param: field name(string), typed_param_value */
        Store_field(param_node, 0, typed_param);
        Store_field(typed_param, 0, caml_copy_string(params[pos].field));

        /* typed_param_value: value with the corresponding type tag */
        switch(params[pos].type) {
        case VIR_TYPED_PARAM_INT:
          typed_param_value = caml_alloc (1, 0);
          v = caml_copy_int32 (params[pos].value.i);
          break;
        case VIR_TYPED_PARAM_UINT:
          typed_param_value = caml_alloc (1, 1);
          v = caml_copy_int32 (params[pos].value.ui);
          break;
        case VIR_TYPED_PARAM_LLONG:
          typed_param_value = caml_alloc (1, 2);
          v = caml_copy_int64 (params[pos].value.l);
          break;
        case VIR_TYPED_PARAM_ULLONG:
          typed_param_value = caml_alloc (1, 3);
          v = caml_copy_int64 (params[pos].value.ul);
          break;
        case VIR_TYPED_PARAM_DOUBLE:
          typed_param_value = caml_alloc (1, 4);
          v = caml_copy_double (params[pos].value.d);
          break;
        case VIR_TYPED_PARAM_BOOLEAN:
          typed_param_value = caml_alloc (1, 5);
          v = Val_bool (params[pos].value.b);
          break;
        case VIR_TYPED_PARAM_STRING:
          typed_param_value = caml_alloc (1, 6);
          v = caml_copy_string (params[pos].value.s);
          free (params[pos].value.s);
          break;
        default:
            /* XXX Memory leak on this path, if there are more
             * VIR_TYPED_PARAM_STRING past this point in the array.
             */
          free (params);
          caml_failwith ("virDomainGetCPUStats: "
                         "unknown parameter type returned");
        }
        Store_field (typed_param_value, 0, v);
        Store_field (typed_param, 1, typed_param_value);
      }
      Store_field (cpustats, cpu + i, param_head);
    }
    cpu += ncpus;
  }
  free(params);
  CAMLreturn (cpustats);
}

CAMLprim value
ocaml_libvirt_domain_migrate_native (value domv, value dconnv, value flagsv, value optdnamev, value opturiv, value optbandwidthv, value unitv)
{
  CAMLparam5 (domv, dconnv, flagsv, optdnamev, opturiv);
  CAMLxparam2 (optbandwidthv, unitv);
  CAMLlocal2 (flagv, rv);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  virConnectPtr dconn = Connect_val (dconnv);
  int flags = 0;
  const char *dname = Optstring_val (optdnamev);
  const char *uri = Optstring_val (opturiv);
  unsigned long bandwidth;
  virDomainPtr r;

  /* Iterate over the list of flags. */
  for (; flagsv != Val_int (0); flagsv = Field (flagsv, 1))
    {
      flagv = Field (flagsv, 0);
      if (flagv == Val_int (0))
	flags |= VIR_MIGRATE_LIVE;
    }

  if (optbandwidthv == Val_int (0)) /* None */
    bandwidth = 0;
  else				/* Some bandwidth */
    bandwidth = Int_val (Field (optbandwidthv, 0));

  NONBLOCKING (r = virDomainMigrate (dom, dconn, flags, dname, uri, bandwidth));
  CHECK_ERROR (!r, conn, "virDomainMigrate");

  rv = Val_domain (r, dconnv);

  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_domain_migrate_bytecode (value *argv, int argn)
{
  return ocaml_libvirt_domain_migrate_native (argv[0], argv[1], argv[2],
					      argv[3], argv[4], argv[5],
					      argv[6]);
}

CAMLprim value
ocaml_libvirt_domain_block_stats (value domv, value pathv)
{
  CAMLparam2 (domv, pathv);
  CAMLlocal2 (rv,v);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  char *path = String_val (pathv);
  struct _virDomainBlockStats stats;
  int r;

  NONBLOCKING (r = virDomainBlockStats (dom, path, &stats, sizeof stats));
  CHECK_ERROR (r == -1, conn, "virDomainBlockStats");

  rv = caml_alloc (5, 0);
  v = caml_copy_int64 (stats.rd_req); Store_field (rv, 0, v);
  v = caml_copy_int64 (stats.rd_bytes); Store_field (rv, 1, v);
  v = caml_copy_int64 (stats.wr_req); Store_field (rv, 2, v);
  v = caml_copy_int64 (stats.wr_bytes); Store_field (rv, 3, v);
  v = caml_copy_int64 (stats.errs); Store_field (rv, 4, v);

  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_domain_interface_stats (value domv, value pathv)
{
  CAMLparam2 (domv, pathv);
  CAMLlocal2 (rv,v);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  char *path = String_val (pathv);
  struct _virDomainInterfaceStats stats;
  int r;

  NONBLOCKING (r = virDomainInterfaceStats (dom, path, &stats, sizeof stats));
  CHECK_ERROR (r == -1, conn, "virDomainInterfaceStats");

  rv = caml_alloc (8, 0);
  v = caml_copy_int64 (stats.rx_bytes); Store_field (rv, 0, v);
  v = caml_copy_int64 (stats.rx_packets); Store_field (rv, 1, v);
  v = caml_copy_int64 (stats.rx_errs); Store_field (rv, 2, v);
  v = caml_copy_int64 (stats.rx_drop); Store_field (rv, 3, v);
  v = caml_copy_int64 (stats.tx_bytes); Store_field (rv, 4, v);
  v = caml_copy_int64 (stats.tx_packets); Store_field (rv, 5, v);
  v = caml_copy_int64 (stats.tx_errs); Store_field (rv, 6, v);
  v = caml_copy_int64 (stats.tx_drop); Store_field (rv, 7, v);

  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_domain_block_peek_native (value domv, value pathv, value offsetv, value sizev, value bufferv, value boffv)
{
  CAMLparam5 (domv, pathv, offsetv, sizev, bufferv);
  CAMLxparam1 (boffv);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  const char *path = String_val (pathv);
  unsigned long long offset = Int64_val (offsetv);
  size_t size = Int_val (sizev);
  char *buffer = String_val (bufferv);
  int boff = Int_val (boffv);
  int r;

  /* Check that the return buffer is big enough. */
  if (caml_string_length (bufferv) < boff + size)
    caml_failwith ("virDomainBlockPeek: return buffer too short");

  /* NB. not NONBLOCKING because buffer might move (XXX) */
  r = virDomainBlockPeek (dom, path, offset, size, buffer+boff, 0);
  CHECK_ERROR (r == -1, conn, "virDomainBlockPeek");

  CAMLreturn (Val_unit);
}

CAMLprim value
ocaml_libvirt_domain_block_peek_bytecode (value *argv, int argn)
{
  return ocaml_libvirt_domain_block_peek_native (argv[0], argv[1], argv[2],
                                                 argv[3], argv[4], argv[5]);
}

CAMLprim value
ocaml_libvirt_domain_memory_peek_native (value domv, value flagsv, value offsetv, value sizev, value bufferv, value boffv)
{
  CAMLparam5 (domv, flagsv, offsetv, sizev, bufferv);
  CAMLxparam1 (boffv);
  CAMLlocal1 (flagv);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  int flags = 0;
  unsigned long long offset = Int64_val (offsetv);
  size_t size = Int_val (sizev);
  char *buffer = String_val (bufferv);
  int boff = Int_val (boffv);
  int r;

  /* Check that the return buffer is big enough. */
  if (caml_string_length (bufferv) < boff + size)
    caml_failwith ("virDomainMemoryPeek: return buffer too short");

  /* Do flags. */
  for (; flagsv != Val_int (0); flagsv = Field (flagsv, 1))
    {
      flagv = Field (flagsv, 0);
      if (flagv == Val_int (0))
        flags |= VIR_MEMORY_VIRTUAL;
    }

  /* NB. not NONBLOCKING because buffer might move (XXX) */
  r = virDomainMemoryPeek (dom, offset, size, buffer+boff, flags);
  CHECK_ERROR (r == -1, conn, "virDomainMemoryPeek");

  CAMLreturn (Val_unit);
}

CAMLprim value
ocaml_libvirt_domain_memory_peek_bytecode (value *argv, int argn)
{
  return ocaml_libvirt_domain_memory_peek_native (argv[0], argv[1], argv[2],
                                                  argv[3], argv[4], argv[5]);
}

/*----------------------------------------------------------------------*/

/* Domain events */

CAMLprim value
ocaml_libvirt_event_register_default_impl (value unitv)
{
  CAMLparam1 (unitv);

  /* arg is of type unit = void */
  int r;

  NONBLOCKING (r = virEventRegisterDefaultImpl ());
  /* must be called before connection, therefore we can't use CHECK_ERROR */
  if (r == -1) caml_failwith("virEventRegisterDefaultImpl");

  CAMLreturn (Val_unit);
}

CAMLprim value
ocaml_libvirt_event_run_default_impl (value unitv)
{
  CAMLparam1 (unitv);

  /* arg is of type unit = void */
  int r;

  NONBLOCKING (r = virEventRunDefaultImpl ());
  if (r == -1) caml_failwith("virEventRunDefaultImpl");

  CAMLreturn (Val_unit);
}

/* We register a single C callback function for every distinct
   callback signature. We encode the signature itself in the function
   name and also in the name of the assocated OCaml callback
   e.g.:
      a C function called
         i_i64_s_callback(virConnectPtr conn,
			  virDomainPtr dom,
			  int x,
			  long y,
			  char *z,
			  void *opaque)
      would correspond to an OCaml callback
         Libvirt.i_i64_s_callback :
	   int64 -> [`R] Domain.t -> int -> int64 -> string option -> unit
      where the initial int64 is a unique ID used by the OCaml to
      dispatch to the specific OCaml closure and stored by libvirt
      as the "opaque" data. */

/* Every one of the callbacks starts with a DOMAIN_CALLBACK_BEGIN(NAME)
   where NAME is the string name of the OCaml callback registered
   in libvirt.ml. */
#define DOMAIN_CALLBACK_BEGIN(NAME)                              \
  value connv, domv, callback_id, result;                        \
  connv = domv = callback_id = result = Val_int(0);              \
  static value *callback = NULL;                                 \
  caml_leave_blocking_section();                                 \
  if (callback == NULL)                                          \
    callback = caml_named_value(NAME);                           \
  if (callback == NULL)                                          \
    abort(); /* C code out of sync with OCaml code */            \
  if ((virDomainRef(dom) == -1) || (virConnectRef(conn) == -1))  \
    abort(); /* should never happen in practice? */              \
                                                                 \
  Begin_roots4(connv, domv, callback_id, result);                \
  connv = Val_connect(conn);                                     \
  domv = Val_domain(dom, connv);                                 \
  callback_id = caml_copy_int64(*(long *)opaque);

/* Every one of the callbacks ends with a CALLBACK_END */
#define DOMAIN_CALLBACK_END                                      \
  (void) caml_callback3(*callback, callback_id, domv, result);   \
  End_roots();                                                   \
  caml_enter_blocking_section();


static void
i_i_callback(virConnectPtr conn,
	     virDomainPtr dom,
	     int x,
	     int y,
	     void * opaque)
{
  DOMAIN_CALLBACK_BEGIN("Libvirt.i_i_callback")
  result = caml_alloc_tuple(2);
  Store_field(result, 0, Val_int(x));
  Store_field(result, 1, Val_int(y));
  DOMAIN_CALLBACK_END
}

static void
u_callback(virConnectPtr conn,
	   virDomainPtr dom,
	   void *opaque)
{
  DOMAIN_CALLBACK_BEGIN("Libvirt.u_callback")
  result = Val_int(0); /* () */
  DOMAIN_CALLBACK_END
}

static void
i64_callback(virConnectPtr conn,
	     virDomainPtr dom,
	     long long int64,
	     void *opaque)
{
  DOMAIN_CALLBACK_BEGIN("Libvirt.i64_callback")
  result = caml_copy_int64(int64);
  DOMAIN_CALLBACK_END
}

static void
i_callback(virConnectPtr conn,
	   virDomainPtr dom,
	   int x,
	   void *opaque)
{
  DOMAIN_CALLBACK_BEGIN("Libvirt.i_callback")
  result = Val_int(x);
  DOMAIN_CALLBACK_END
}

static void
s_i_callback(virConnectPtr conn,
	     virDomainPtr dom,
	     char *x,
	     int y,
	     void * opaque)
{
  DOMAIN_CALLBACK_BEGIN("Libvirt.s_i_callback")
  result = caml_alloc_tuple(2);
  Store_field(result, 0, 
	      Val_opt(x, (Val_ptr_t) caml_copy_string));
  Store_field(result, 1, Val_int(y));
  DOMAIN_CALLBACK_END
}

static void
s_i_i_callback(virConnectPtr conn,
	       virDomainPtr dom,
	       char *x,
	       int y,
	       int z,
	       void * opaque)
{
  DOMAIN_CALLBACK_BEGIN("Libvirt.s_i_i_callback")
  result = caml_alloc_tuple(3);
  Store_field(result, 0, 
	      Val_opt(x, (Val_ptr_t) caml_copy_string));
  Store_field(result, 1, Val_int(y));
  Store_field(result, 2, Val_int(z));
  DOMAIN_CALLBACK_END
}

static void
s_s_i_callback(virConnectPtr conn,
	       virDomainPtr dom,
	       char *x,
	       char *y,
	       int z,
	       void *opaque)
{
  DOMAIN_CALLBACK_BEGIN("Libvirt.s_s_i_callback")
  result = caml_alloc_tuple(3);
  Store_field(result, 0, 
	      Val_opt(x, (Val_ptr_t) caml_copy_string));
  Store_field(result, 1,
	      Val_opt(y, (Val_ptr_t) caml_copy_string));
  Store_field(result, 2, Val_int(z));
  DOMAIN_CALLBACK_END
}

static void
s_s_i_s_callback(virConnectPtr conn,
		 virDomainPtr dom,
		 char *x,
		 char *y,
		 int z,
		 char *a,
		 void *opaque)
{
  DOMAIN_CALLBACK_BEGIN("Libvirt.s_s_i_s_callback")
  result = caml_alloc_tuple(4);
  Store_field(result, 0, 
	      Val_opt(x, (Val_ptr_t) caml_copy_string));
  Store_field(result, 1,
	      Val_opt(y, (Val_ptr_t) caml_copy_string));
  Store_field(result, 2, Val_int(z));
  Store_field(result, 3,
	      Val_opt(a, (Val_ptr_t) caml_copy_string));
  DOMAIN_CALLBACK_END
}

static void
s_s_s_i_callback(virConnectPtr conn,
		 virDomainPtr dom,
		 char * x,
		 char * y,
		 char * z,
		 int a,
		 void * opaque)
{
  DOMAIN_CALLBACK_BEGIN("Libvirt.s_s_s_i_callback")
  result = caml_alloc_tuple(4);
  Store_field(result, 0,
	      Val_opt(x, (Val_ptr_t) caml_copy_string));
  Store_field(result, 1,
              Val_opt(y, (Val_ptr_t) caml_copy_string));
  Store_field(result, 2,
              Val_opt(z, (Val_ptr_t) caml_copy_string));
  Store_field(result, 3, Val_int(a));
  DOMAIN_CALLBACK_END
}

static value
Val_event_graphics_address(virDomainEventGraphicsAddressPtr x)
{
  CAMLparam0 ();
  CAMLlocal1(result);
  result = caml_alloc_tuple(3);
  Store_field(result, 0, Val_int(x->family));
  Store_field(result, 1,
	      Val_opt((void *) x->node, (Val_ptr_t) caml_copy_string));
  Store_field(result, 2,
	      Val_opt((void *) x->service, (Val_ptr_t) caml_copy_string));
  CAMLreturn(result);
}

static value
Val_event_graphics_subject_identity(virDomainEventGraphicsSubjectIdentityPtr x)
{
  CAMLparam0 ();
  CAMLlocal1(result);
  result = caml_alloc_tuple(2);
  Store_field(result, 0,
	      Val_opt((void *) x->type, (Val_ptr_t) caml_copy_string));
  Store_field(result, 1,
	      Val_opt((void *) x->name, (Val_ptr_t) caml_copy_string));
  CAMLreturn(result);

}

static value
Val_event_graphics_subject(virDomainEventGraphicsSubjectPtr x)
{
  CAMLparam0 ();
  CAMLlocal1(result);
  int i;
  result = caml_alloc_tuple(x->nidentity);
  for (i = 0; i < x->nidentity; i++ )
    Store_field(result, i,
		Val_event_graphics_subject_identity(x->identities + i));
  CAMLreturn(result);
}

static void
i_ga_ga_s_gs_callback(virConnectPtr conn,
		      virDomainPtr dom,
		      int i1,
		      virDomainEventGraphicsAddressPtr ga1,
		      virDomainEventGraphicsAddressPtr ga2,
		      char *s1,
		      virDomainEventGraphicsSubjectPtr gs1,
		      void * opaque)
{
  DOMAIN_CALLBACK_BEGIN("Libvirt.i_ga_ga_s_gs_callback")
  result = caml_alloc_tuple(5);
  Store_field(result, 0, Val_int(i1));
  Store_field(result, 1, Val_event_graphics_address(ga1));
  Store_field(result, 2, Val_event_graphics_address(ga2)); 
  Store_field(result, 3,
	      Val_opt(s1, (Val_ptr_t) caml_copy_string));
  Store_field(result, 4, Val_event_graphics_subject(gs1));
  DOMAIN_CALLBACK_END
}

static void
timeout_callback(int timer, void *opaque)
{
  value callback_id, result;
  callback_id = result = Val_int(0);
  static value *callback = NULL;
  caml_leave_blocking_section();
  if (callback == NULL)
    callback = caml_named_value("Libvirt.timeout_callback");
  if (callback == NULL)
    abort(); /* C code out of sync with OCaml code */

  Begin_roots2(callback_id, result);
  callback_id = caml_copy_int64(*(long *)opaque);

  (void)caml_callback_exn(*callback, callback_id);
  End_roots();
  caml_enter_blocking_section();
}

CAMLprim value
ocaml_libvirt_event_add_timeout (value connv, value ms, value callback_id)
{
  CAMLparam3 (connv, ms, callback_id);
  virConnectPtr conn = Connect_val (connv);
  void *opaque;
  virFreeCallback freecb = free;
  virEventTimeoutCallback cb = timeout_callback;

  int r;

  /* Store the int64 callback_id as the opaque data so the OCaml
     callback can demultiplex to the correct OCaml handler. */
  if ((opaque = malloc(sizeof(long))) == NULL)
    caml_failwith ("virEventAddTimeout: malloc");
  *((long*)opaque) = Int64_val(callback_id);
  NONBLOCKING(r = virEventAddTimeout(Int_val(ms), cb, opaque, freecb));
  CHECK_ERROR(r == -1, conn, "virEventAddTimeout");

  CAMLreturn(Val_int(r));
}

CAMLprim value
ocaml_libvirt_event_remove_timeout (value connv, value timer_id)
{
  CAMLparam2 (connv, timer_id);
  virConnectPtr conn = Connect_val (connv);
  int r;

  NONBLOCKING(r = virEventRemoveTimeout(Int_val(timer_id)));
  CHECK_ERROR(r == -1, conn, "virEventRemoveTimeout");

  CAMLreturn(Val_int(r));
}

CAMLprim value
ocaml_libvirt_connect_domain_event_register_any(value connv, value domv, value callback, value callback_id)
{
  CAMLparam4(connv, domv, callback, callback_id);

  virConnectPtr conn = Connect_val (connv);
  virDomainPtr dom = NULL;
  int eventID = Tag_val(callback);

  virConnectDomainEventGenericCallback cb;
  void *opaque;
  virFreeCallback freecb = free;
  int r;

  if (domv != Val_int(0))
    dom = Domain_val (Field(domv, 0));

  switch (eventID){
  case VIR_DOMAIN_EVENT_ID_LIFECYCLE:
    cb = VIR_DOMAIN_EVENT_CALLBACK(i_i_callback);
    break;
  case VIR_DOMAIN_EVENT_ID_REBOOT:
    cb = VIR_DOMAIN_EVENT_CALLBACK(u_callback);
    break;
  case VIR_DOMAIN_EVENT_ID_RTC_CHANGE:
    cb = VIR_DOMAIN_EVENT_CALLBACK(i64_callback);
    break;
  case VIR_DOMAIN_EVENT_ID_WATCHDOG:
    cb = VIR_DOMAIN_EVENT_CALLBACK(i_callback);
    break;
  case VIR_DOMAIN_EVENT_ID_IO_ERROR:
    cb = VIR_DOMAIN_EVENT_CALLBACK(s_s_i_callback);
    break;
  case VIR_DOMAIN_EVENT_ID_GRAPHICS:
    cb = VIR_DOMAIN_EVENT_CALLBACK(i_ga_ga_s_gs_callback);
    break;
  case VIR_DOMAIN_EVENT_ID_IO_ERROR_REASON:
    cb = VIR_DOMAIN_EVENT_CALLBACK(s_s_i_s_callback);
    break;
  case VIR_DOMAIN_EVENT_ID_CONTROL_ERROR:
    cb = VIR_DOMAIN_EVENT_CALLBACK(u_callback);
    break;
  case VIR_DOMAIN_EVENT_ID_BLOCK_JOB:
    cb = VIR_DOMAIN_EVENT_CALLBACK(s_i_i_callback);
    break;
  case VIR_DOMAIN_EVENT_ID_DISK_CHANGE:
    cb = VIR_DOMAIN_EVENT_CALLBACK(s_s_s_i_callback);
    break;
  case VIR_DOMAIN_EVENT_ID_TRAY_CHANGE:
    cb = VIR_DOMAIN_EVENT_CALLBACK(s_i_callback);
    break;
  case VIR_DOMAIN_EVENT_ID_PMWAKEUP:
    cb = VIR_DOMAIN_EVENT_CALLBACK(i_callback);
    break;
  case VIR_DOMAIN_EVENT_ID_PMSUSPEND:
    cb = VIR_DOMAIN_EVENT_CALLBACK(i_callback);
    break;
  case VIR_DOMAIN_EVENT_ID_BALLOON_CHANGE:
    cb = VIR_DOMAIN_EVENT_CALLBACK(i64_callback);
    break;
  case VIR_DOMAIN_EVENT_ID_PMSUSPEND_DISK:
    cb = VIR_DOMAIN_EVENT_CALLBACK(i_callback);
    break;
  default:
    caml_failwith("vifConnectDomainEventRegisterAny: unimplemented eventID");
  }

  /* Store the int64 callback_id as the opaque data so the OCaml
     callback can demultiplex to the correct OCaml handler. */
  if ((opaque = malloc(sizeof(long))) == NULL)
    caml_failwith ("virConnectDomainEventRegisterAny: malloc");
  *((long*)opaque) = Int64_val(callback_id);
  NONBLOCKING(r = virConnectDomainEventRegisterAny(conn, dom, eventID, cb, opaque, freecb));
  CHECK_ERROR(r == -1, conn, "virConnectDomainEventRegisterAny");

  CAMLreturn(Val_int(r));
}

CAMLprim value
ocaml_libvirt_storage_pool_get_info (value poolv)
{
  CAMLparam1 (poolv);
  CAMLlocal2 (rv, v);
  virStoragePoolPtr pool = Pool_val (poolv);
  virConnectPtr conn = Connect_polv (poolv);
  virStoragePoolInfo info;
  int r;

  NONBLOCKING (r = virStoragePoolGetInfo (pool, &info));
  CHECK_ERROR (r == -1, conn, "virStoragePoolGetInfo");

  rv = caml_alloc (4, 0);
  Store_field (rv, 0, Val_int (info.state));
  v = caml_copy_int64 (info.capacity); Store_field (rv, 1, v);
  v = caml_copy_int64 (info.allocation); Store_field (rv, 2, v);
  v = caml_copy_int64 (info.available); Store_field (rv, 3, v);

  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_storage_vol_get_info (value volv)
{
  CAMLparam1 (volv);
  CAMLlocal2 (rv, v);
  virStorageVolPtr vol = Volume_val (volv);
  virConnectPtr conn = Connect_volv (volv);
  virStorageVolInfo info;
  int r;

  NONBLOCKING (r = virStorageVolGetInfo (vol, &info));
  CHECK_ERROR (r == -1, conn, "virStorageVolGetInfo");

  rv = caml_alloc (3, 0);
  Store_field (rv, 0, Val_int (info.type));
  v = caml_copy_int64 (info.capacity); Store_field (rv, 1, v);
  v = caml_copy_int64 (info.allocation); Store_field (rv, 2, v);

  CAMLreturn (rv);
}

/*----------------------------------------------------------------------*/

CAMLprim value
ocaml_libvirt_virterror_get_last_error (value unitv)
{
  CAMLparam1 (unitv);
  CAMLlocal1 (rv);
  virErrorPtr err = virGetLastError ();

  rv = Val_opt (err, (Val_ptr_t) Val_virterror);

  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_virterror_get_last_conn_error (value connv)
{
  CAMLparam1 (connv);
  CAMLlocal1 (rv);
  virConnectPtr conn = Connect_val (connv);

  rv = Val_opt (conn, (Val_ptr_t) Val_connect);

  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_virterror_reset_last_error (value unitv)
{
  CAMLparam1 (unitv);
  virResetLastError ();
  CAMLreturn (Val_unit);
}

CAMLprim value
ocaml_libvirt_virterror_reset_last_conn_error (value connv)
{
  CAMLparam1 (connv);
  virConnectPtr conn = Connect_val (connv);
  virConnResetLastError (conn);
  CAMLreturn (Val_unit);
}

/*----------------------------------------------------------------------*/

/* Initialise the library. */
CAMLprim value
ocaml_libvirt_init (value unit)
{
  CAMLparam1 (unit);
  CAMLlocal1 (rv);
  int r;

  r = virInitialize ();
  CHECK_ERROR (r == -1, NULL, "virInitialize");

  CAMLreturn (Val_unit);
}
