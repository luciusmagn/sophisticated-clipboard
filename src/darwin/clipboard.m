#import <AppKit/AppKit.h>
#import <Foundation/Foundation.h>

#if __cplusplus
extern "C" {
#endif

char** getClipboardTypes(int* count) {
    NSPasteboard *pb = [NSPasteboard generalPasteboard];
    NSArray<NSPasteboardType> *types = [pb types];

    *count = (int)[types count];
    if (*count == 0) return NULL;

    char **result = malloc(*count * sizeof(char*));
    for (int i = 0; i < *count; i++) {
        NSString *type = types[i];
        const char *utf8 = [type UTF8String];
        result[i] = malloc(strlen(utf8) + 1);
        strcpy(result[i], utf8);
    }
    return result;
}

void freeClipboardTypes(char** types, int count) {
    if (!types) return;
    for (int i = 0; i < count; i++) {
        free(types[i]);
    }
    free(types);
}

int hasClipboardType(const char* type) {
    NSPasteboard *pb = [NSPasteboard generalPasteboard];
    NSString *nsType = [NSString stringWithUTF8String:type];
    return [pb availableTypeFromArray:@[nsType]] != nil ? 1 : 0;
}

unsigned char* getClipboardData(const char* type, int* length) {
    NSPasteboard *pb = [NSPasteboard generalPasteboard];
    NSString *nsType = [NSString stringWithUTF8String:type];

    if ([nsType isEqualToString:NSPasteboardTypeString] ||
        [nsType hasSuffix:@"text/plain"] ||
        [nsType hasSuffix:@"text/html"]) {
        NSString *string = [pb stringForType:nsType];
        if (string) {
            const char *utf8 = [string UTF8String];
            *length = (int)strlen(utf8);
            unsigned char *result = malloc(*length);
            memcpy(result, utf8, *length);
            return result;
        }
    }

    NSData *data = [pb dataForType:nsType];
    if (!data) {
        *length = 0;
        return NULL;
    }

    *length = (int)[data length];
    unsigned char *result = malloc(*length);
    memcpy(result, [data bytes], *length);
    return result;
}

int setClipboardData(const char* type, const unsigned char* data, int length) {
    NSPasteboard *pb = [NSPasteboard generalPasteboard];
    NSString *nsType = [NSString stringWithUTF8String:type];

    static BOOL cleared = NO;
    if (!cleared) {
        [pb clearContents];
        cleared = YES;
    }

    if ([nsType isEqualToString:NSPasteboardTypeString] ||
        [nsType hasSuffix:@"text/plain"] ||
        [nsType hasSuffix:@"text/html"]) {
        NSString *string = [[NSString alloc] initWithBytes:data
                                                    length:length
                                                  encoding:NSUTF8StringEncoding];
        if (string) {
            return [pb setString:string forType:nsType] ? 1 : 0;
        }
    }

    NSData *nsData = [NSData dataWithBytes:data length:length];
    return [pb setData:nsData forType:nsType] ? 1 : 0;
}

void resetClipboard(void) {
    static BOOL *cleared_ptr = NULL;
    if (!cleared_ptr) {
        // (this is a hack, i don't know if it works)
        extern BOOL cleared;
        cleared_ptr = &cleared;
    }
    *cleared_ptr = NO;
}

#if __cplusplus
}
#endif
