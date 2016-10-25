
/^\(define-module / {
    for (;;) {
        if ((getline line) <= 0)
            break
        if (line ~ /^ *[^ #].*\)\)$/)
            break
    }
    next
}

/^ *\"- Scheme.*[^\\]\"$/ { next }
/^ *\"- Scheme/ {
    for (;;) {
        if ((getline line) <= 0)
            break
        if (line ~ /^.*[^\\]\"$/)
            break
    }
    next
}

{ print }
