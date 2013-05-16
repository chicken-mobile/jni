package com.chicken_mobile.jni;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

public class ReflectionHelper {
	
	public static Field[] findFields(Class<?> c) {
		Set<Field> fields = new HashSet<Field>();
		for (Field m : c.getFields()) {
			fields.add(m);
		}
		for (Field m : c.getDeclaredFields()) {
			fields.add(m);
		}
		return fields.toArray(new Field[fields.size()]);
	}
	
	public static Method[] findMethods(Class<?> c) {
		Set<Method> methods = new HashSet<Method>();
		for (Method m : c.getMethods()) {
			methods.add(m);
		}
		for (Method m : c.getDeclaredMethods()) {
			methods.add(m);
		}
		return methods.toArray(new Method[methods.size()]);
	}
	
	public static Method[] findMethodsByName(Class<?> c, String name) {
		ArrayList<Method> methods = new ArrayList<Method>();
		for (Method m : ReflectionHelper.findMethods(c)) {
			if (m.getName().equals(name)) {
				methods.add(m);
			}
		}
		Method[] ms = new Method[methods.size()];
		return methods.toArray(ms);
	}
	
	public static Field findField(Class<?> c, String name) {
		try {
			for (Field f : ReflectionHelper.findFields(c)) {
				if (f.getName().equals(name))
					return f;
			}
			return null;
		} catch (SecurityException e) {
			return null;
		}
	}
}