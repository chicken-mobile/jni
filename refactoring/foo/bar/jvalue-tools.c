
/* int *ptr; */
/* ptr = malloc(10 * sizeof (*ptr));               /\* without a cast *\/ */
/* ptr = (int *)malloc(10 * sizeof (*ptr));        /\* with a cast *\/ */


jvalue* make_jvalue_array(int length)
{
  return malloc(length * sizeof(jvalue));
}

void free_jvalue_array(jvalue* values)
{
  free(values);
}

int jvalue_array_length(jvalue* values)
{
  return sizeof(values)/sizeof(jvalue);
}

jvalue* set_boolean_jvalue(jvalue* values, int idx, jboolean value)
{
  values[idx].z = value;
  return values;
}

jvalue* set_byte_jvalue(jvalue* values, int idx, jbyte value)
{
  values[idx].b = value;
  return values;
}

jvalue* set_char_jvalue(jvalue* values, int idx, jchar value)
{
  values[idx].c = value;
  return values;
}

jvalue* set_short_jvalue(jvalue* values, int idx, jshort value)
{
  values[idx].s = value;
  return values;
}

jvalue* set_int_jvalue(jvalue* values, int idx, jint value)
{
  values[idx].i = value;
  return values;
}

jvalue* set_long_jvalue(jvalue* values, int idx, jlong value)
{
  values[idx].j = value;
  return values;
}

jvalue* set_float_jvalue(jvalue* values, int idx, jfloat value)
{
  values[idx].f = value;
  return values;
}

jvalue* set_double_jvalue(jvalue* values, int idx, jdouble value)
{
  values[idx].d = value;
  return values;
}

jvalue* set_object_jvalue(jvalue* values, int idx, jobject value)
{
  values[idx].l = value;
  return values;
}
