package edu.zhangfan.compiler;

import com.google.common.collect.ImmutableSet;

import java.util.function.Predicate;

public class Utils {

    static private <E> Predicate<E> oneOf(E... terminals) {
        ImmutableSet<E> terminalSet = ImmutableSet.copyOf(terminals);
        return terminalSet::contains;
    }
}
