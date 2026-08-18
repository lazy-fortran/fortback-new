#!/usr/bin/env python3
"""Generate the fixed MIR-v0 bounded RISC-V Linux bridge policy."""

from pathlib import Path
import sys


ROOT = Path(__file__).resolve().parents[1]
INPUT = ROOT / "spec" / "mir_v0_riscv_linux_bridge_policy.tsv"
OUTPUT = ROOT / "src" / "fortback_mir_v0_riscv_linux_bridge_policy.f90"


def read_policy():
    policy = {"storage-policy": None, "instruction-count": None, "instructions": [],
              "result-shapes": [], "source-rules": [], "literal-ranges": [],
              "source-literals": [], "source-literal-sequences": [],
              "frame-operation": None, "exit-status-operation": None, "route-operations": []}
    shape_names = set()
    for line_number, line in enumerate(INPUT.read_text().splitlines(), 1):
        fields = line.split()
        if not fields or fields[0].startswith("#"):
            continue
        if (fields[0] == "storage-policy" and len(fields) == 13 and fields[1] == "key" and
                fields[3] == "offset" and fields[5] == "frame-size" and
                fields[7] == "initialization" and fields[9] == "load-operation" and
                fields[11] == "store-operation"):
            if policy["storage-policy"] is not None:
                raise SystemExit(f"{INPUT}:{line_number}: duplicate storage policy")
            policy["storage-policy"] = (fields[2], int(fields[4]), int(fields[6]),
                                          int(fields[8]), fields[10], fields[12])
        elif fields[0] == "instruction-count" and len(fields) == 3 and fields[1] == "value":
            if policy["instruction-count"] is not None:
                raise SystemExit(f"{INPUT}:{line_number}: duplicate instruction count")
            policy["instruction-count"] = int(fields[2])
        elif fields[0] == "instruction" and len(fields) == 5 and fields[1] == "opcode":
            policy["instructions"].append((int(fields[2]), fields[3], fields[4]))
        elif fields[0] == "frame-operation" and len(fields) == 2:
            policy["frame-operation"] = fields[1]
        elif fields[0] == "exit-status-operation" and len(fields) == 2:
            policy["exit-status-operation"] = fields[1]
        elif (fields[0] == "route-operation" and len(fields) == 7 and
              fields[1] == "source-rule" and fields[3] == "index" and
              fields[5] == "operation"):
            policy["route-operations"].append((fields[2], int(fields[4]), fields[6]))
        elif (fields[0] == "route-pattern" and len(fields) == 7 and
              fields[1] == "source-rule" and fields[3] == "count" and
              fields[5] == "pattern" and fields[6] == "storage-sequence"):
            count = int(fields[4])
            operations = [(0, "addi"), (1, "sd")]
            for index in range(2, count - 1):
                operations.append(((index), ("ld", "addi", "add", "sd")[(index - 2) % 4]))
            operations.append((count - 1, "addi"))
            policy["route-operations"].extend(
                (fields[2], index, operation) for index, operation in operations)
        elif fields[0] == "result-shape" and len(fields) == 5:
            if fields[1] in shape_names:
                raise SystemExit(f"{INPUT}:{line_number}: duplicate result shape")
            shape_names.add(fields[1])
            policy["result-shapes"].append(tuple(fields[1:]))
        elif (fields[0] == "literal-range" and len(fields) == 7 and
              fields[1] == "opcode" and fields[3] == "min" and fields[5] == "max"):
            opcode = fields[2]
            minimum = int(fields[4])
            maximum = int(fields[6])
            if minimum > maximum:
                raise SystemExit(f"{INPUT}:{line_number}: literal range minimum exceeds maximum")
            if any(existing_opcode == opcode for existing_opcode, _, _ in policy["literal-ranges"]):
                raise SystemExit(f"{INPUT}:{line_number}: duplicate literal range")
            policy["literal-ranges"].append((opcode, minimum, maximum))
        elif (fields[0] == "source-literal" and len(fields) in (9, 11) and
              fields[1] == "function" and fields[3] == "source-rule" and
              fields[5] == "opcode"):
            if len(fields) == 9 and fields[7] == "value":
                instruction_index = 0
                literal = int(fields[8])
            elif (len(fields) == 11 and fields[7] == "index" and
                  fields[9] == "value"):
                instruction_index = int(fields[8])
                literal = int(fields[10])
            else:
                raise SystemExit(f"{INPUT}:{line_number}: malformed source literal")
            policy["source-literals"].append(
                (fields[2], fields[4], fields[6], instruction_index, literal))
        elif (fields[0] == "source-literal-sequence" and len(fields) == 7 and
              fields[1] == "function" and fields[3] == "source-rule" and
              fields[5] == "values"):
            values = tuple(int(value) for value in fields[6].split(','))
            if not values:
                raise SystemExit(f"{INPUT}:{line_number}: empty source literal sequence")
            policy["source-literal-sequences"].append(
                (fields[2], fields[4], values))
        elif fields[0] == "source-rule" and len(fields) >= 6 and fields[1] == "function":
            shapes = tuple(fields[4].split(","))
            if any(shape not in shape_names for shape in shapes):
                raise SystemExit(f"{INPUT}:{line_number}: unknown result shape")
            opcodes = tuple(fields[5:])
            if len(opcodes) != len(policy["instructions"]):
                if not opcodes:
                    opcodes = tuple(opcode for _, opcode, _ in policy["instructions"])
            if fields[3].startswith("frontend-ast-v1/storage-sequence-"):
                sequence_number = int(fields[3].rsplit("-", 1)[1])
                if sequence_number >= 7:
                    shapes = ["integer-literal-left", "integer-sequence-store-literal"]
                    for step in range(2, sequence_number + 1):
                        suffix = "" if step == 2 else f"-{step}"
                        shapes.extend((f"integer-sequence{suffix}-loaded",
                                       f"integer-sequence{suffix}-literal-right",
                                       f"integer-sequence{suffix}-expression",
                                       f"integer-sequence{suffix}-expression-result"))
                    shapes.append(shapes[-1])
                    shapes = tuple(shapes)
            if len(shapes) not in (1, len(opcodes)):
                raise SystemExit(f"{INPUT}:{line_number}: result shape route length mismatch")
            if len(shapes) == 1:
                shapes = shapes * len(opcodes)
            policy["source-rules"].append((fields[2], fields[3], shapes, opcodes))
        else:
            raise SystemExit(f"{INPUT}:{line_number}: malformed row")
    if policy["instruction-count"] != len(policy["instructions"]):
        raise SystemExit(f"{INPUT}: instruction count does not match instruction rows")
    if not policy["result-shapes"]:
        raise SystemExit(f"{INPUT}: at least one result-shape row is required")
    if not policy["source-rules"]:
        raise SystemExit(f"{INPUT}: at least one source-rule row is required")
    if policy["storage-policy"] is None:
        raise SystemExit(f"{INPUT}: storage policy is required")
    if policy["frame-operation"] is None or policy["exit-status-operation"] is None:
        raise SystemExit(f"{INPUT}: frame and exit-status operations are required")
    return policy


def opcode_constant(name):
    return "mir_v0_opcode_" + name.replace("-", "_")


def render(policy):
    storage_key, storage_offset, frame_size, initialization, load_operation, store_operation = policy["storage-policy"]
    source_rules_by_function = {}
    for function_name, source_rule, shape_names, opcodes in policy["source-rules"]:
        source_rules_by_function.setdefault(function_name, {}).setdefault(source_rule, []).append(
            (shape_names, opcodes))
    route_operations = {}
    for source_rule, index, operation in policy["route-operations"]:
        route_operations.setdefault(source_rule, {})[index] = operation
    source_literals = {}
    for function_name, source_rule, opcode, instruction_index, literal in policy["source-literals"]:
        source_literals.setdefault(function_name, {}).setdefault(source_rule, []).append(
            (opcode, instruction_index, literal))
    source_literal_sequences = {}
    for function_name, source_rule, values in policy["source-literal-sequences"]:
        source_literal_sequences.setdefault(function_name, {}).setdefault(source_rule, []).append(values)

    supported_opcodes = []
    for _, opcode, _ in policy["instructions"]:
        if opcode not in supported_opcodes:
            supported_opcodes.append(opcode)
    for _, _, _, opcodes in policy["source-rules"]:
        for opcode in opcodes:
            if opcode not in supported_opcodes:
                supported_opcodes.append(opcode)
    opcode_imports = (", &" + chr(10) + "        ").join(
        opcode_constant(opcode) for opcode in supported_opcodes)
    lines = [
        "! Generated by scripts/generate_mir_v0_riscv_linux_bridge_policy.py; do not edit.",
        "module fortback_mir_v0_riscv_linux_bridge_policy",
        "    use iso_fortran_env, only: int32",
        f"    use fortback_mir_v0_bridge_metadata, only: {opcode_imports}, &",
        "        mir_v0_value_kind_complex, &",
        "        mir_v0_value_kind_integer, mir_v0_value_kind_logical, &",
        "        mir_v0_value_kind_real, mir_v0_value_kind_character",
        "    implicit none",
        "    private",
        "",
        f"    integer(int32), parameter, public :: mir_v0_bridge_policy_instruction_count = {policy['instruction-count']}_int32",
        f"    integer(int32), parameter, public :: mir_v0_bridge_policy_result_shape_count = {len(policy['result-shapes'])}_int32",
        f"    character(len=16), parameter, public :: mir_v0_bridge_policy_storage_key = '{storage_key}'",
        f"    integer(int32), parameter, public :: mir_v0_bridge_policy_storage_offset = {storage_offset}_int32",
        f"    integer(int32), parameter, public :: mir_v0_bridge_policy_frame_size = {frame_size}_int32",
        f"    integer(int32), parameter, public :: mir_v0_bridge_policy_storage_initialization = {initialization}_int32",
        f"    character(len=16), parameter, public :: mir_v0_bridge_policy_load_operation = '{load_operation}'",
        f"    character(len=16), parameter, public :: mir_v0_bridge_policy_store_operation = '{store_operation}'",
        "",
        "    public :: mir_v0_bridge_policy_accepts",
        "    public :: mir_v0_bridge_policy_function_supported",
        "    public :: mir_v0_bridge_policy_opcode_supported",
        "    public :: mir_v0_bridge_policy_instruction_count_for",
        "    public :: mir_v0_bridge_policy_instruction_count_matches",
        "    public :: mir_v0_bridge_policy_machine_operation_for",
        "    public :: mir_v0_bridge_policy_frame_operation",
        "    public :: mir_v0_bridge_policy_exit_status_operation",
        "    public :: mir_v0_bridge_policy_route_operation_for",
        "    public :: mir_v0_bridge_policy_storage_matches",
        "",
        "contains",
        "",
        "    pure logical function mir_v0_bridge_policy_storage_matches(storage_present, storage_key)",
        "        logical, intent(in) :: storage_present",
        "        character(len=*), intent(in) :: storage_key",
        "",
        "        mir_v0_bridge_policy_storage_matches = .not. storage_present",
        "        if (storage_present) mir_v0_bridge_policy_storage_matches = &",
        "            trim(storage_key) == trim(mir_v0_bridge_policy_storage_key)",
        "    end function mir_v0_bridge_policy_storage_matches",
        "",
        "    pure logical function mir_v0_bridge_policy_result_shape_matches(shape_name, &",
        "            result_id, result_kind, result_type)",
        "        character(len=*), intent(in) :: shape_name, result_type",
        "        integer(int32), intent(in) :: result_id, result_kind",
        "",
        "        mir_v0_bridge_policy_result_shape_matches = .false.",
        "        select case (trim(shape_name))",
    ]
    for shape_name, result_id, result_kind, result_type in policy["result-shapes"]:
        kind_constant = "mir_v0_value_kind_" + result_kind
        lines += [f"        case ('{shape_name}')", f"            if (result_id /= {result_id}_int32) return", f"            if (result_kind /= {kind_constant}) return", f"            if (trim(result_type) /= '{result_type}') return", "            mir_v0_bridge_policy_result_shape_matches = .true."]
    lines += [
        "        case default",
        "            return",
        "        end select",
        "    end function mir_v0_bridge_policy_result_shape_matches",
        "",
        "    pure logical function mir_v0_bridge_policy_function_supported(function_name)",
        "        character(len=*), intent(in) :: function_name",
        "",
        "        select case (trim(function_name))",
    ]
    for function_name in source_rules_by_function:
        lines += [f"        case ('{function_name}')", "            mir_v0_bridge_policy_function_supported = .true."]
    lines += [
        "        case default",
        "            mir_v0_bridge_policy_function_supported = .false.",
        "        end select",
        "    end function mir_v0_bridge_policy_function_supported",
        "",
        "    pure logical function mir_v0_bridge_policy_opcode_supported(opcode)",
        "        integer(int32), intent(in) :: opcode",
        "",
        "        select case (opcode)",
    ]
    for opcode in supported_opcodes:
        lines += [f"        case ({opcode_constant(opcode)})", "            mir_v0_bridge_policy_opcode_supported = .true."]
    lines += [
        "        case default",
        "            mir_v0_bridge_policy_opcode_supported = .false.",
        "        end select",
        "    end function mir_v0_bridge_policy_opcode_supported",
        "",
        "    pure function mir_v0_bridge_policy_machine_operation_for(opcode) result(operation)",
        "        integer(int32), intent(in) :: opcode",
        "        character(len=16) :: operation",
        "",
        "        operation = ''",
        "        select case (opcode)",
    ]
    for _, opcode, operation in policy["instructions"]:
        lines += [f"        case ({opcode_constant(opcode)})", f"            operation = '{operation}'"]
    lines += [
        "        end select",
        "    end function mir_v0_bridge_policy_machine_operation_for",
        "",
        "    pure function mir_v0_bridge_policy_frame_operation() result(operation)",
        "        character(len=16) :: operation",
        f"        operation = '{policy['frame-operation']}'",
        "    end function mir_v0_bridge_policy_frame_operation",
        "",
        "    pure function mir_v0_bridge_policy_exit_status_operation() result(operation)",
        "        character(len=16) :: operation",
        f"        operation = '{policy['exit-status-operation']}'",
        "    end function mir_v0_bridge_policy_exit_status_operation",
        "",
        "    pure function mir_v0_bridge_policy_route_operation_for(source_rule, index) result(operation)",
        "        character(len=*), intent(in) :: source_rule",
        "        integer(int32), intent(in) :: index",
        "        character(len=16) :: operation",
        "        operation = ''",
        "        select case (trim(source_rule))",
    ]
    for source_rule, operations in route_operations.items():
        lines += [f"        case ('{source_rule}')", "            select case (index)"]
        for index, operation in sorted(operations.items()):
            lines += [f"            case ({index}_int32)", f"                operation = '{operation}'"]
        lines += ["            end select"]
    lines += [
        "        end select",
        "    end function mir_v0_bridge_policy_route_operation_for",
        "",
        "    pure integer(int32) function mir_v0_bridge_policy_instruction_count_for( &",
        "            function_name, source_rule)",
        "        character(len=*), intent(in) :: function_name, source_rule",
        "",
        "        mir_v0_bridge_policy_instruction_count_for = 0_int32",
        "        select case (trim(function_name))",
    ]
    for function_name, source_rules in source_rules_by_function.items():
        lines += [f"        case ('{function_name}')",
                  "            select case (trim(source_rule))"]
        for source_rule, routes in source_rules.items():
            lines += [f"            case ('{source_rule}')",
                      f"                mir_v0_bridge_policy_instruction_count_for = {len(routes[0][1])}_int32"]
        lines += ["            end select"]
    lines += [
        "        end select",
        "    end function mir_v0_bridge_policy_instruction_count_for",
        "",
        "    pure logical function mir_v0_bridge_policy_instruction_count_matches( &",
        "            function_name, source_rule, instruction_count)",
        "        character(len=*), intent(in) :: function_name, source_rule",
        "        integer(int32), intent(in) :: instruction_count",
        "",
        "        mir_v0_bridge_policy_instruction_count_matches = .false.",
        "        select case (trim(function_name))",
    ]
    for function_name, source_rules in source_rules_by_function.items():
        lines += [f"        case ('{function_name}')", "            select case (trim(source_rule))"]
        for source_rule, routes in source_rules.items():
            lines += [f"            case ('{source_rule}')"]
            for _, opcodes in routes:
                lines += [f"                if (instruction_count == {len(opcodes)}_int32) then",
                          "                    mir_v0_bridge_policy_instruction_count_matches = .true.",
                          "                    return", "                end if"]
        lines += ["            end select"]
    lines += [
        "        end select",
        "    end function mir_v0_bridge_policy_instruction_count_matches",
        "",
        "    pure logical function mir_v0_bridge_policy_accepts(function_name, &",
        "            instruction_count, instruction_index, opcode, result_id, result_kind, &",
        "            result_type, source_rule, literal_present, literal)",
        "        character(len=*), intent(in) :: function_name, result_type, source_rule",
        "        logical, intent(in) :: literal_present",
        "        integer(int32), intent(in) :: instruction_count, instruction_index, opcode, &",
        "            result_id, result_kind, literal",
        "",
        "        mir_v0_bridge_policy_accepts = .false.",
        "        if (instruction_index < 0_int32 .or. instruction_index >= instruction_count) return",
        "        select case (trim(function_name))",
    ]
    for function_name, source_rules in source_rules_by_function.items():
        lines += [f"        case ('{function_name}')", "            select case (trim(source_rule))"]
        for source_rule, routes in source_rules.items():
            lines += [f"            case ('{source_rule}')"]
            lines += ["                if (.not. mir_v0_bridge_policy_instruction_count_matches( &",
                      "                    function_name, source_rule, instruction_count)) return",
                      "                select case (instruction_count)"]
            for route_length in sorted({len(opcodes) for _, opcodes in routes}):
                matching_routes = [(shapes, opcodes) for shapes, opcodes in routes
                                   if len(opcodes) == route_length]
                lines += [f"                case ({route_length}_int32)",
                          "                    select case (instruction_index)"]
                for index in range(route_length):
                    shapes_by_opcode = {}
                    for shapes, opcodes in matching_routes:
                        shapes_by_opcode.setdefault(opcodes[index], []).append(shapes[index])
                    lines += [f"                    case ({index}_int32)"]
                    shape_sets = {tuple(sorted(set(shapes))) for shapes in shapes_by_opcode.values()}
                    if len(shape_sets) == 1:
                        opcodes = sorted(shapes_by_opcode)
                        opcode_check = " .and. ".join(
                            f"opcode /= {opcode_constant(opcode)}" for opcode in opcodes)
                        shapes_at_index = []
                        for shapes, _ in matching_routes:
                            if shapes[index] not in shapes_at_index:
                                shapes_at_index.append(shapes[index])
                        lines += [f"                        if ({opcode_check}) return"]
                        for shape_index, shape in enumerate(shapes_at_index):
                            prefix = "if" if shape_index == 0 else "else if"
                            continuation_indent = "                            " if shape_index == 0 else "                                "
                            lines += [f"                        {prefix} (mir_v0_bridge_policy_result_shape_matches( &",
                                      f"{continuation_indent}'{shape}', result_id, result_kind, result_type)) then"]
                        lines += ["                        else", "                            return", "                        end if"]
                    else:
                        for opcode_index, (opcode, shapes) in enumerate(sorted(shapes_by_opcode.items())):
                            prefix = "if" if opcode_index == 0 else "else if"
                            lines += [f"                        {prefix} (opcode == {opcode_constant(opcode)}) then"]
                            for shape_index, shape in enumerate(sorted(set(shapes))):
                                prefix = "if" if shape_index == 0 else "else if"
                                lines += [f"                            {prefix} (mir_v0_bridge_policy_result_shape_matches( &",
                                          f"                                '{shape}', result_id, result_kind, result_type)) then"]
                            lines += ["                            else", "                                return", "                            end if"]
                        lines += ["                        else", "                            return", "                        end if"]
                lines += ["                    case default", "                        return", "                    end select"]
            lines += ["                case default", "                    return", "                end select",
                      "                select case (opcode)"]
            for opcode, minimum, maximum in policy["literal-ranges"]:
                lines += [f"                case ({opcode_constant(opcode)})",
                          "                    if (.not. literal_present) return",
                          f"                    if (literal < {minimum}_int32 .or. literal > {maximum}_int32) return"]
            lines += ["                case default", "                    if (literal_present) return", "                end select"]
            literal_alternatives = {}
            for opcode, instruction_index, literal in source_literals.get(function_name, {}).get(source_rule, []):
                literal_alternatives.setdefault((opcode, instruction_index), []).append(literal)
            for (opcode, instruction_index), literals in literal_alternatives.items():
                count_guard = ''
                if (function_name == 'main' and source_rule == 'frontend-ast-v2/execution-part' and
                        instruction_index == 0):
                    count_guard = ' .and. instruction_count == 5_int32'
                if (function_name == 'p' and source_rule == 'frontend-ast-v2/print-stmt' and
                        instruction_index in (0, 2, 4)):
                    count_guard = ' .and. instruction_count /= 7_int32'
                    if instruction_index == 0 and literals == [7]:
                        lines += [f"                if (opcode == {opcode_constant(opcode)} .and. instruction_index == {instruction_index}_int32 .and. instruction_count /= 7_int32 .and. instruction_count /= 5_int32 .and. literal /= 7_int32) return",
                                  f"                if (opcode == {opcode_constant(opcode)} .and. instruction_index == {instruction_index}_int32 .and. instruction_count == 5_int32 .and. literal /= 7_int32 .and. literal /= 17_int32) return"]
                        continue
                alternatives = ' .and. '.join(f'literal /= {literal}_int32' for literal in literals)
                lines += [f"                if (opcode == {opcode_constant(opcode)} .and. instruction_index == {instruction_index}_int32{count_guard} .and. {alternatives}) return"]
            seen_sequence_lengths = set()
            for values in source_literal_sequences.get(function_name, {}).get(source_rule, []):
                if len(values) in seen_sequence_lengths:
                    continue
                seen_sequence_lengths.add(len(values))
                lines += ["                if (opcode == mir_v0_opcode_const .and. instruction_count == " +
                          f"{2 * len(values) + 1}_int32) then",
                          "                    select case (instruction_index)"]
                for index, value in enumerate(values):
                    alternatives = [str(sequence[index]) for sequence in
                                    source_literal_sequences[function_name][source_rule]
                                    if len(sequence) == len(values)]
                    lines += [f"                    case ({2 * index}_int32)",
                              f"                        if ({' .and. '.join(f'literal /= {alternative}_int32' for alternative in alternatives)}) return"]
                lines += ["                    case default", "                        return",
                          "                    end select", "                end if"]
        lines += ["            case default", "                return", "            end select"]
    lines += ["        case default", "            return", "        end select", "        mir_v0_bridge_policy_accepts = .true.", "    end function mir_v0_bridge_policy_accepts", "", "end module fortback_mir_v0_riscv_linux_bridge_policy", ""]
    return chr(10).join(lines)


def main():
    policy = read_policy()
    generated = render(policy)
    if "--check" in sys.argv:
        current = OUTPUT.read_text() if OUTPUT.exists() else ""
        if current != generated:
            raise SystemExit(f"{OUTPUT} is stale; run {Path(__file__).name}")
    else:
        OUTPUT.write_text(generated)


if __name__ == "__main__":
    main()
