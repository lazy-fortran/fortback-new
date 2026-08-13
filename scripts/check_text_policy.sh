#!/usr/bin/env bash
# Enforce the no-accumulator-concatenation rule on Fortran sources.
#
# An allocatable character is allowed at an explicitly documented boundary:
#     character(:), allocatable :: value  ! text-policy: C boundary
#
# Usage:
#   check_text_policy.sh [DIR ...]
#   check_text_policy.sh --self-test

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

scan() {
    local dirs=("$@") found=0 f line no
    local files=()

    for d in "${dirs[@]}"; do
        [ -d "$d" ] || continue
        while IFS= read -r f; do files+=("$f"); done \
            < <(find "$d" \( -name '*.f90' -o -name '*.inc' \) -type f | sort)
    done
    [ ${#files[@]} -gt 0 ] || return 0

    for f in "${files[@]}"; do
        while IFS=: read -r no line; do
            [ -n "$no" ] || continue
            printf '%s:%s: accumulator concatenation (D0011)\n' "$f" "$no"
            printf '    %s\n' "$(printf '%s' "$line" | sed 's/^[[:space:]]*//')"
            found=1
        done < <(grep -nE '^[^!]*\b([a-zA-Z_][a-zA-Z0-9_%]*)[[:space:]]*=[^=]*\b\1\b[^=]*//' "$f" || true)

        while IFS=: read -r no line; do
            [ -n "$no" ] || continue
            case "$line" in *text-policy:*) continue ;; esac
            printf '%s:%s: allocatable character buffer (D0011)\n' "$f" "$no"
            printf '    %s\n' "$(printf '%s' "$line" | sed 's/^[[:space:]]*//')"
            found=1
        done < <(grep -nE '^[^!]*character\((len=)?:\)[[:space:]]*,[[:space:]]*allocatable' "$f" || true)
    done
    return "$found"
}

self_test() {
    local tmp rc
    tmp=$(mktemp -d)
    trap 'rm -rf "$tmp"' RETURN
    mkdir -p "$tmp/bad" "$tmp/good"

    cat > "$tmp/bad/violation.f90" <<'EOF'
module violation
contains
    subroutine build(out)
        character(:), allocatable :: out
        out = out//'x'
    end subroutine build
end module violation
EOF

    cat > "$tmp/good/compliant.f90" <<'EOF'
module compliant
    use iso_fortran_env, only: int8
    type :: byte_buffer_t
        integer(int8), allocatable :: data(:)
    end type byte_buffer_t
contains
    subroutine append(buffer, value)
        type(byte_buffer_t), intent(inout) :: buffer
        character(len=*), intent(in) :: value
        character(:), allocatable :: boundary  ! text-policy: C boundary
        boundary = value
        buffer%data = [buffer%data, int(iachar(boundary), int8)]
    end subroutine append
end module compliant
EOF

    rc=0
    scan "$tmp/bad" >/dev/null 2>&1 || rc=$?
    if [ "$rc" -eq 0 ]; then
        printf '%s\n' 'SELF-TEST FAILED: violation was accepted' >&2
        return 1
    fi

    rc=0
    scan "$tmp/good" >/dev/null 2>&1 || rc=$?
    if [ "$rc" -ne 0 ]; then
        printf '%s\n' 'SELF-TEST FAILED: compliant source was rejected' >&2
        return 1
    fi

    printf '%s\n' 'text-policy checker self-test: ok'
}

if [ "${1:-}" = --self-test ]; then
    self_test
    exit $?
fi

cd "$ROOT"
dirs=("$@")
[ ${#dirs[@]} -gt 0 ] || dirs=(src app)

rc=0
scan "${dirs[@]}" || rc=$?
if [ "$rc" -eq 0 ]; then
    printf 'text policy: clean (%s)\n' "${dirs[*]}"
else
    printf '%s\n' 'text policy: violations found' >&2
fi
exit "$rc"
