input = readLines("C:\\Users\\volan\\Documents\\Advent of Code 2021\\16\\input.txt")

hex_string_to_bit_string = function(string){
  #https://stackoverflow.com/questions/6614283/converting-decimal-to-binary-in-r
  hex_chars = unlist(strsplit(string, ''))
  hex_digits = strtoi(hex_chars, base = 16)
  binary_strings = sapply(
    hex_digits, Vectorize(function(int){
      paste0(as.integer(rev(intToBits(int)[1:4])), collapse = '')
    })
  )
  return(paste0(binary_strings, collapse = ''))
}

concat_and_parse_binary = function(bit_vector){
  strtoi(paste0(bit_vector, collapse = ''), base = 2)
}

initial_bit_vector = unlist(strsplit(hex_string_to_bit_string(input), ''))

get_first_n_bits = function(input_bit_vector,n){
  bits = input_bit_vector[1:n]
  if(length(input_bit_vector) > n){
    new_bit_vector = input_bit_vector[(n+1):length(input_bit_vector)]
  } else {
    new_bit_vector = input_bit_vector[FALSE]
  }
  return(list(bits = bits, bit_vector = new_bit_vector))
}

# A packet is a list with type - number (4 is literal), version (number), data (number or list of packets)
# Parsing functions return a packet and the remaining vector (in a list)

parse_literal = function(bit_vector){
  current_number = 0
  continue = '1'
  while (continue == '1'){
    five_bits_list = get_first_n_bits(bit_vector, 5)
    five_bits = five_bits_list$bits
    bit_vector = five_bits_list$bit_vector
    continue = five_bits[1]
    number_to_add = concat_and_parse_binary(five_bits[2:5])
    current_number = current_number * 16 + number_to_add
  }
  return(list(bit_vector = bit_vector, value = current_number))
}

parse_packet = function(bit_vector){
  version_bits_list = get_first_n_bits(bit_vector, 3)
  version_bits = version_bits_list$bits
  bit_vector = version_bits_list$bit_vector
  version = concat_and_parse_binary(version_bits)
  type_bits_list = get_first_n_bits(bit_vector, 3)
  type_bits = type_bits_list$bits
  bit_vector = type_bits_list$bit_vector
  type = concat_and_parse_binary(type_bits)
  if(type == 4){
    parsed_literal = parse_literal(bit_vector)
    packet = list(version = version, type = type, data = parsed_literal$value)
    return(list(bit_vector = parsed_literal$bit_vector, packet = packet))
  } else {
    parsed_subpackets = parse_subpackets(bit_vector)
    packet = list(version = version, type = type, data = parsed_subpackets$subpackets_list)
    return(list(bit_vector = parsed_subpackets$bit_vector, packet = packet))
  }
}

parse_subpackets = function(bit_vector){
  subpackets_list = list()
  lengthtypeid_list = get_first_n_bits(bit_vector, 1)
  lengthtypeid = lengthtypeid_list$bits
  bit_vector = lengthtypeid_list$bit_vector
  if(lengthtypeid == '0'){
    length_of_subpackets_bits_list = get_first_n_bits(bit_vector, 15)
    length_of_subpackets_bits = length_of_subpackets_bits_list$bits
    bit_vector = length_of_subpackets_bits_list$bit_vector
    length_of_subpackets = strtoi(paste0(length_of_subpackets_bits, collapse = ''), base = 2)
    subpacket_bits_list = get_first_n_bits(bit_vector, length_of_subpackets)
    subpacket_bits = subpacket_bits_list$bits
    bit_vector = subpacket_bits_list$bit_vector
    list_entry = 1
    while(length(subpacket_bits) > 0){
      new_parsed_packet = parse_packet(subpacket_bits)
      subpacket_bits = new_parsed_packet$bit_vector
      subpackets_list[[list_entry]] = new_parsed_packet$packet
      list_entry = list_entry + 1
    }
  } else {
    count_of_subpackets_bits_list = get_first_n_bits(bit_vector, 11)
    count_of_subpackets_bits = count_of_subpackets_bits_list$bits
    bit_vector = count_of_subpackets_bits_list$bit_vector
    count_of_subpackets = strtoi(paste0(count_of_subpackets_bits, collapse = ''), base = 2)
    for(list_entry in 1:count_of_subpackets){
      new_parsed_packet = parse_packet(bit_vector)
      bit_vector = new_parsed_packet$bit_vector
      subpackets_list[[list_entry]] = new_parsed_packet$packet
    }
  }
  
  return(list(bit_vector = bit_vector, subpackets_list = subpackets_list))
}

parsed_packet_list = parse_packet(initial_bit_vector)

test_parse = function(string){
  initial_bit_vector = unlist(strsplit(hex_string_to_bit_string(string), ''))
  parse_packet(initial_bit_vector)
}

test_parse('8A004A801A8002F478')

unlisted_parsed_packet_list = unlist(parsed_packet_list$packet)
versions = unlisted_parsed_packet_list[grepl('version', names(unlisted_parsed_packet_list))]
sum(versions) #897

evaluate_packet = function(packet){
  type = packet$type
  if(type == 4){
    return(packet$data)
  } else {
    values = unlist(lapply(packet$data, evaluate_packet))
    if(type == 0){
      return(sum(values))
    } else if (type == 1){
      return(prod(values))
    } else if (type == 2){
      return(min(values))
    } else if (type == 3){
      return(max(values))
    } else if (type == 5){
      if(values[1] > values[2] & length(values) == 2){
        return(1)
      } else if (values[1] <= values[2] & length(values) == 2){
        return(0)
      } else {stop()}
    } else if (type == 6){
      if(values[1] < values[2] & length(values) == 2){
        return(1)
      } else if (values[1] >= values[2] & length(values) == 2){
        return(0)
      } else {stop()}
    } else if (type == 7){
      if(values[1] == values[2] & length(values) == 2){
        return(1)
      } else if (values[1] != values[2] & length(values) == 2){
        return(0)
      } else {stop()}
    } else {stop()}
  }
}

test_evaluate_parse = function(string){
  evaluate_packet(test_parse(string)$packet)
}

test_evaluate_parse('9C005AC2F8F0')

sprintf('%20.0f', evaluate_packet(parsed_packet_list$packet)) #9485076995911

        