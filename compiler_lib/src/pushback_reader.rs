use std::io::Read;
use std::io::Result;

pub struct PushbackReader<'a, T: Read + 'a> {
    reader: &'a mut T,
    buffer: Vec<u8>,
}

impl<'a, T: Read + 'a> Read for PushbackReader<'a, T> {
    fn read(&mut self, dest: &mut [u8]) -> Result<usize> {
        let dest_size = dest.len();
        let buf_size = self.buffer.len();

        if buf_size <= dest_size {
            self.buffer.reverse();
            dest[0..buf_size].copy_from_slice(self.buffer.as_slice());
            self.buffer.clear();
            let read_from_reader = self.reader.read(&mut dest[buf_size..])?;
            return Ok(read_from_reader + buf_size);
        } else {
            let buf_part_iter = self.buffer.drain(buf_size - dest_size..buf_size).rev();
            dest.copy_from_slice(buf_part_iter.collect::<Vec<u8>>().as_slice());
            Ok(dest_size)
        }
    }
}

impl<'a, T: Read + 'a> PushbackReader<'a, T> {
    pub fn create(r: &'a mut T) -> PushbackReader<'a, T> {
        PushbackReader {
            reader: r,
            buffer: Vec::new(),
        }
    }

    pub fn unread(&mut self, bs: &[u8]) {
        let orig_buf_size = self.buffer.len();
        self.buffer.extend_from_slice(bs);
        let actual_buf_size = self.buffer.len();
        self.buffer[orig_buf_size..actual_buf_size].reverse();
    }

    pub fn unread_byte(&mut self, b: u8) {
        self.unread(&[b]);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read_sanity_check() {
        let vec: Vec<u8> = vec![1, 2, 3];
        let mut slice = vec.as_slice();
        let mut pbr = PushbackReader::create(&mut slice);

        let mut buf = Vec::new();
        assert_eq!(pbr.read_to_end(&mut buf).unwrap(), 3);
        assert_eq!(buf, [1, 2, 3]);
    }

    #[test]
    fn unread_sanity_check() {
        let vec: Vec<u8> = vec![];
        let mut slice = vec.as_slice();
        let mut pbr = PushbackReader::create(&mut slice);

        pbr.unread_byte(1);
        pbr.unread_byte(2);
        pbr.unread_byte(3);

        let mut buf = Vec::new();
        assert_eq!(pbr.read_to_end(&mut buf).unwrap(), 3);
        assert_eq!(buf, [3, 2, 1]);
    }

    #[test]
    fn buf_size_le_dest_size() {
        let vec: Vec<u8> = vec![1, 2, 3];
        let mut slice = vec.as_slice();
        let mut pbr = PushbackReader::create(&mut slice);

        pbr.unread_byte(10);
        pbr.unread_byte(20);
        pbr.unread_byte(30);
        // buf size = 3 at this moment

        let mut buf: [u8; 5] = [0; 5];
        assert_eq!(pbr.read(&mut buf).unwrap(), 5);
        assert_eq!(buf, [30, 20, 10, 1, 2]);
    }

    #[test]
    fn buf_size_gt_dest_size() {
        let vec: Vec<u8> = vec![1, 2, 3];
        let mut slice = vec.as_slice();
        let mut pbr = PushbackReader::create(&mut slice);

        pbr.unread_byte(10);
        pbr.unread_byte(20);
        pbr.unread_byte(30);
        pbr.unread_byte(40);
        pbr.unread_byte(50);
        // buf size = 5 at this moment

        let mut buf: [u8; 3] = [0; 3];
        assert_eq!(pbr.read(&mut buf).unwrap(), 3);
        assert_eq!(buf, [50, 40, 30]);

        let mut buf: [u8; 2] = [0; 2];
        assert_eq!(pbr.read(&mut buf).unwrap(), 2);
        assert_eq!(buf, [20, 10]);

        let mut buf: [u8; 3] = [0; 3];
        assert_eq!(pbr.read(&mut buf).unwrap(), 3);
        assert_eq!(buf, [1, 2, 3]);
    }
}
